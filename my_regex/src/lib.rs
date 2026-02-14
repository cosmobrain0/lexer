use std::collections::HashSet;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Character {
    Char(char),
    EndOfInput,
}
impl From<char> for Character {
    fn from(value: char) -> Self {
        Self::Char(value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Matcher {
    Char(Character),
    Optional(Box<Matcher>),
    Star(Box<Matcher>),
    Concat(Box<Matcher>, Box<Matcher>),
    Selection(HashSet<Character>),
}
impl Matcher {
    pub fn new(regex: &str) -> Option<Self> {
        let mut chars = regex.chars().rev().collect::<Vec<_>>();
        let result = Self::new_from_chars(&mut chars);
        chars.is_empty().then_some(result).flatten()
    }

    fn new_from_chars(chars: &mut Vec<char>) -> Option<Self> {
        let mut regexes = vec![];
        let mut escaped = false;
        while let Some(c) = chars.pop() {
            match (c, escaped) {
                ('*', false) => {
                    if let Some(regex) = regexes.pop() {
                        regexes.push(Matcher::Star(Box::new(regex)));
                    } else {
                        return None;
                    }
                }
                ('?', false) => {
                    if let Some(regex) = regexes.pop() {
                        regexes.push(Matcher::Optional(Box::new(regex)));
                    }
                }
                ('(', false) => {
                    if let Some(regex) = Self::new_from_chars(chars)
                        && let Some(')') = chars.pop()
                    {
                        regexes.push(regex);
                    } else {
                        return None;
                    }
                }
                (')', false) => {
                    chars.push(')');
                    break;
                }
                ('[', false) => {
                    let mut selection = vec![];
                    let mut found_end = false;
                    while let Some(c) = chars.pop() {
                        if c == ']' {
                            found_end = true;
                            break;
                        } else if c == '\\' {
                            let next = chars.pop();
                            if let Some(']') = next {
                                selection.push(Character::Char(']'));
                            } else if next == Some('$') {
                                selection.push(Character::Char('$'));
                            } else {
                                return None;
                            }
                        } else if c == '$' {
                            selection.push(Character::EndOfInput);
                        } else {
                            selection.push(Character::Char(c));
                        }
                    }
                    if !found_end {
                        return None;
                    }
                    regexes.push(Matcher::Selection(HashSet::from_iter(selection)));
                }
                (']', false) => return None,
                ('$', false) => {
                    regexes.push(Matcher::Char(Character::EndOfInput));
                }
                (c, _) => {
                    regexes.push(Matcher::Char(c.into()));
                }
            }
            escaped = false;
        }

        regexes
            .into_iter()
            .reduce(|acc, val| Matcher::Concat(Box::new(acc), Box::new(val)))
    }

    pub fn match_from_start(&self, text: &str) -> HashSet<String> {
        match self {
            Matcher::Char(expected) => {
                if let Character::Char(expected_character) = expected
                    && let Some(c) = text.chars().next()
                    && c == *expected_character
                {
                    let mut set = HashSet::with_capacity(1);
                    set.insert(c.to_string());
                    set
                } else if *expected == Character::EndOfInput && text.is_empty() {
                    HashSet::from_iter([String::new()])
                } else {
                    HashSet::new()
                }
            }
            Matcher::Optional(regex_matcher) => {
                let mut matches = regex_matcher.match_from_start(text);
                matches.insert(String::new());
                matches
            }
            Matcher::Star(regex_matcher) => {
                let mut matches = HashSet::new();
                matches.insert(String::new());
                let mut previous_matches = vec![String::new()];
                while let Some(prior) = previous_matches.pop() {
                    let remainder = &text[prior.len()..];
                    let new_matches = regex_matcher.match_from_start(remainder);
                    for new_match in new_matches {
                        matches.insert(format!("{p}{n}", p = &prior, n = &new_match));
                        if !new_match.is_empty() {
                            previous_matches.push(format!("{p}{new_match}", p = &prior));
                        }
                    }
                }
                matches
            }
            Matcher::Concat(regex_matcher, regex_matcher1) => {
                let mut results = HashSet::new();

                let first_matches = regex_matcher.match_from_start(text);
                for first_match in first_matches {
                    let remainder = &text[first_match.len()..];
                    let second_matches = regex_matcher1.match_from_start(remainder);
                    for second_match in second_matches {
                        results.insert(format!("{first}{second_match}", first = &first_match));
                    }
                }

                results
            }
            Matcher::Selection(items) => {
                if let Some(expected) = text.chars().next()
                    && items.contains(&Character::Char(expected))
                {
                    let mut set = HashSet::new();
                    set.insert(expected.to_string());
                    set
                } else if text.is_empty() && items.contains(&Character::EndOfInput) {
                    HashSet::from_iter([String::new()])
                } else {
                    HashSet::new()
                }
            }
        }
    }

    pub fn longest_match_from_start(&self, text: &str) -> Option<String> {
        self.match_from_start(text).into_iter().reduce(|acc, val| {
            if acc.chars().count() > val.chars().count() {
                acc
            } else {
                val
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use crate::{Character, Matcher};

    // TODO: add a bunch of tests!
    #[test]
    fn test_char() {
        let should_match = "c";
        let should_not_match = "d";
        let regex = Matcher::Char(Character::Char('c'));
        let expectation_should_match = HashSet::from_iter([String::from("c")]);
        let expectation_should_not_match = HashSet::new();

        let result_should_match = regex.match_from_start(should_match);
        let result_should_not_match = regex.match_from_start(should_not_match);

        assert_eq!(result_should_match, expectation_should_match);
        assert_eq!(result_should_not_match, expectation_should_not_match);
    }

    #[test]
    fn test_end_of_input() {
        let regex = Matcher::Char(Character::EndOfInput);
        let tests = [("", true), ("c", false)];
        for (input, expectation) in tests {
            assert_eq!(!regex.match_from_start(input).is_empty(), expectation);
        }
    }

    #[test]
    fn test_optional() {
        let regex = Matcher::Optional(Box::new(Matcher::Char('c'.into())));
        let tests = [
            ("", HashSet::from_iter([String::from("")])),
            (
                "c",
                HashSet::from_iter([String::from(""), String::from("c")]),
            ),
            ("d", HashSet::from_iter([String::from("")])),
        ];

        for (input, expectation) in tests {
            assert_eq!(expectation, regex.match_from_start(input));
        }
    }

    #[test]
    fn test_star() {
        let regex = Matcher::Star(Box::new(Matcher::Char('c'.into())));
        let tests = [
            ("", vec![""]),
            ("c", vec!["", "c"]),
            ("ccccc", vec!["", "c", "cc", "ccc", "cccc", "ccccc"]),
        ];

        for (input, expectation) in tests {
            assert_eq!(
                regex.match_from_start(input),
                HashSet::from_iter(expectation.into_iter().map(String::from))
            );
        }
    }

    #[test]
    fn test_selection() {
        let regex = Matcher::Selection(HashSet::from_iter([
            'a'.into(),
            'b'.into(),
            'c'.into(),
            'e'.into(),
        ]));
        let tests = [
            ("a", true),
            ("b", true),
            ("c", true),
            ("d", false),
            ("e", true),
            ("", false),
        ];

        for (input, expectation) in tests {
            assert_eq!(expectation, !regex.match_from_start(input).is_empty());
        }
    }

    #[test]
    fn test_concat() {
        let regex = Matcher::Concat(
            Box::new(Matcher::Char('c'.into())),
            Box::new(Matcher::Char('d'.into())),
        );

        assert!(!regex.match_from_start("cd").is_empty());
        assert!(regex.match_from_start("c").is_empty());
        assert!(regex.match_from_start("d").is_empty());
        assert!(regex.match_from_start("dc").is_empty());
    }

    #[test]
    fn test_sequence() {
        // "a(b*c)?[efg]"
        let regex = Matcher::Concat(
            Box::new(Matcher::Concat(
                Box::new(Matcher::Char('a'.into())),
                Box::new(Matcher::Optional(Box::new(Matcher::Concat(
                    Box::new(Matcher::Star(Box::new(Matcher::Char('b'.into())))),
                    Box::new(Matcher::Char('c'.into())),
                )))),
            )),
            Box::new(Matcher::Selection(HashSet::from_iter([
                'e'.into(),
                'f'.into(),
                'g'.into(),
            ]))),
        );

        let tests = [
            ("abce", true),
            ("abbbe", false),
            ("abbbbbce", true),
            ("acf", true),
            ("ag", true),
            ("bcf", false),
            ("af", true),
            ("ab", false),
        ];

        for (input, expectation) in tests {
            assert_eq!(expectation, !dbg!(regex.match_from_start(input)).is_empty())
        }
    }

    #[test]
    fn test_constructor() {
        let tests = [
            ("", None),
            ("a", Some(Matcher::Char('a'.into()))),
            (
                "a?",
                Some(Matcher::Optional(Box::new(Matcher::Char('a'.into())))),
            ),
            (
                "a*",
                Some(Matcher::Star(Box::new(Matcher::Char('a'.into())))),
            ),
            (
                "[abc]",
                Some(Matcher::Selection(HashSet::from_iter([
                    'a'.into(),
                    'b'.into(),
                    'c'.into(),
                ]))),
            ),
            (
                "ab?c*[defg]",
                Some(Matcher::Concat(
                    Box::new(Matcher::Concat(
                        Box::new(Matcher::Concat(
                            Box::new(Matcher::Char('a'.into())),
                            Box::new(Matcher::Optional(Box::new(Matcher::Char('b'.into())))),
                        )),
                        Box::new(Matcher::Star(Box::new(Matcher::Char('c'.into())))),
                    )),
                    Box::new(Matcher::Selection(HashSet::from_iter([
                        'd'.into(),
                        'e'.into(),
                        'f'.into(),
                        'g'.into(),
                    ]))),
                )),
            ),
            (
                "([+-])",
                Some(Matcher::Selection(HashSet::from_iter([
                    '+'.into(),
                    '-'.into(),
                ]))),
            ),
        ];

        for (input, expectation) in tests {
            assert_eq!(dbg!(expectation), dbg!(Matcher::new(input)));
        }
    }
}
