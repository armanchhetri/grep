use std::{collections::HashMap, iter::Peekable};

pub fn search<'a>(content: &'a str, pattern: &str) -> Vec<&'a str> {
    let filtered: Vec<&str> = content
        .lines()
        .filter(|line| match_it(line, pattern))
        .collect();
    for line in &filtered {
        println!("{line}");
    }
    filtered
}

fn match_it(line: &str, pattern: &str) -> bool {
    let line_vec: Vec<char> = line.chars().collect();
    let mut line_it = line_vec.iter().peekable();
    let mut capture_group: HashMap<i32, String> = HashMap::new();

    let (matched_seq, _) = match_line(&mut line_it, pattern, true, &mut capture_group, 0, None);
    return !matched_seq.is_empty();
}

#[derive(PartialEq)]
enum Memory {
    Value(char),
    PositiveGroup(String),
    NegativeGroup(String),
    Alphabetic,
    Numeric,
    None,
}

fn match_line<'a, I>(
    line_it: &mut Peekable<I>,
    pattern: &str,
    mut first_match: bool,
    capture_group: &mut HashMap<i32, String>,
    curr_idx: i32,
    next_in_pattern: Option<&char>, // this is to handle the greedy case such as ([^abc]+),
) -> (String, i32)
where
    I: Iterator<Item = &'a char>,
    I: Clone,
{
    let mut next_idx = curr_idx;
    let mut pattern_it = pattern.chars().peekable();
    let mut matched_seq = String::new();
    let mut memory: Memory = Memory::None;

    while let Some(p) = pattern_it.next() {
        let mut temp_memory = Memory::None;
        let matches = match p {
            '[' => {
                let mut matches = false;
                let (group_chars, neg) = get_group_chars(&mut pattern_it);
                if neg {
                    matches = true;
                    for c in &mut *line_it {
                        // the iteration is to handle the first match
                        if group_chars.contains(*c) {
                            matches = false;
                            break;
                        }
                        if !first_match {
                            matched_seq.push(*c);
                            matches = true;
                            break;
                        }
                        matched_seq.push(*c);
                    }
                    temp_memory = Memory::NegativeGroup(group_chars);
                } else {
                    for c in &mut *line_it {
                        if group_chars.contains(*c) {
                            matched_seq.push(*c);
                            matches = true;
                            break;
                        }
                        if !first_match {
                            matches = false;
                            break;
                        }
                    }
                    temp_memory = Memory::PositiveGroup(group_chars);
                }

                first_match = false;
                matches
            }
            '\\' => {
                let mut matches = false;
                if let Some(directive) = pattern_it.next() {
                    if directive.is_numeric() {
                        let index: i32 = directive.to_digit(10).unwrap() as i32;
                        assert!(index > 0);
                        let pattern = capture_group.get(&index).unwrap().clone();
                        let random_negative_number = -100;
                        let (matched_substring, _) = match_line(
                            line_it,
                            &pattern,
                            false,
                            capture_group,
                            random_negative_number,
                            next_in_pattern,
                        );

                        matched_seq.push_str(&matched_substring);
                        matches = !matched_substring.is_empty()
                    } else {
                        while let Some(c) = line_it.next() {
                            matches = match directive {
                                'd' => {
                                    temp_memory = Memory::Numeric;
                                    c.is_ascii_digit()
                                }

                                'w' => {
                                    temp_memory = Memory::Alphabetic;
                                    c.is_alphabetic()
                                }
                                _ => false,
                            };

                            if matches || !first_match {
                                matched_seq.push(*c);
                                break;
                            }
                        }
                    }
                }
                first_match = false;
                matches
            }
            '(' => {
                next_idx += 1;
                let sub_patterns = get_all_sub_patterns(&mut pattern_it);
                let next_in_pattern = pattern_it.peek(); // for greedy match

                let mut matches = false;

                for pattern in sub_patterns {
                    let matched_substring;
                    let mut copied_line_it = line_it.clone();

                    (matched_substring, next_idx) = match_line(
                        &mut copied_line_it,
                        &pattern,
                        first_match,
                        capture_group,
                        next_idx,
                        next_in_pattern,
                    );

                    if !matched_substring.is_empty() {
                        matched_seq.push_str(&matched_substring);
                        let read_count = matched_substring.len();
                        line_it.nth(read_count - 1);
                        matches = true;
                        break;
                    }
                }

                first_match = false;
                matches
            }
            '^' => {
                first_match = false;
                true
            }
            '$' => {
                first_match = false;
                line_it.next().is_none()
            }
            '.' => {
                first_match = false;

                let mut matches = false;
                if let Some(c) = line_it.next() {
                    matched_seq.push(*c);
                    matches = true;
                }
                matches
            }
            '+' => {
                if first_match {
                    false
                } else {
                    first_match = false;
                    let one_or_more_match = match_one_or_more(line_it, memory, next_in_pattern);
                    matched_seq.push_str(&one_or_more_match);
                    true
                }
            }
            '?' => {
                first_match = false;
                true
            }
            other => {
                let (matched_raw_chars, matches) =
                    match_one_raw_char(line_it, &mut pattern_it, other, first_match);

                matched_seq.push_str(&matched_raw_chars);
                first_match = false;
                matches
            }
        };
        if temp_memory == Memory::None {
            memory = Memory::Value(p);
        } else {
            memory = temp_memory;
        }
        if !matches {
            return (String::new(), next_idx);
        }
    }

    if curr_idx > 0 {
        capture_group.insert(curr_idx, matched_seq.clone());
    }

    return (matched_seq, next_idx);
}

fn get_group_chars(pattern_it: &mut impl Iterator<Item = char>) -> (String, bool) {
    let mut neg = false;
    let mut group_chars = String::new();
    for group_char in &mut *pattern_it {
        if group_char == '^' {
            neg = true;
            continue;
        }
        if group_char == ']' {
            break;
        }
        group_chars.push(group_char);
    }
    (group_chars, neg)
}

fn match_one_raw_char<'a, I, J>(
    line_it: &mut Peekable<I>,
    pattern_it: &mut Peekable<J>,
    raw_char: char,
    first_match: bool,
) -> (String, bool)
where
    I: Iterator<Item = &'a char>,
    J: Iterator<Item = char>,
{
    let mut matched_seq = String::new();
    let mut matches = false;
    while let Some(c) = line_it.peek() {
        if **c == raw_char {
            matches = true;
            matched_seq.push(*line_it.next().unwrap());
            break;
        }
        if let Some(question) = pattern_it.peek() {
            if *question == '?' {
                pattern_it.next();
                matches = true;
                break;
            }
        }
        if !first_match {
            matched_seq.push(*line_it.next().unwrap());
            matches = false;
            break;
        }
        matched_seq.push(*line_it.next().unwrap());
    }

    if let Some(question) = pattern_it.peek() {
        if *question == '?' {
            pattern_it.next();
            matches = true;
        }
    }

    (matched_seq, matches)
}

fn match_one_or_more<'a, I>(
    line_it: &mut Peekable<I>,
    memory: Memory,
    next_in_pattern: Option<&char>,
) -> String
where
    I: Iterator<Item = &'a char>,
{
    let mut matched_seq = String::new();
    while let Some(c) = line_it.peek() {
        let has_match = match memory {
            Memory::Value(m) => m == **c,
            Memory::Alphabetic => c.is_alphabetic(),
            Memory::Numeric => c.is_numeric(),
            Memory::PositiveGroup(ref g) => g.contains(**c),
            Memory::NegativeGroup(ref g) => match next_in_pattern {
                Some(next_in_pat) => !g.contains(**c) && *next_in_pat != **c,
                _ => !g.contains(**c),
            },
            Memory::None => false,
        };
        if !has_match {
            break;
        }
        matched_seq.push(*line_it.next().unwrap());
    }
    matched_seq
}

// expected input
// abc|cde|efg) i.e omitting first `(`
fn get_all_sub_patterns(pattern_it: &mut impl Iterator<Item = char>) -> Vec<String> {
    let mut bracket_stack: Vec<char> = Vec::from(['(']);

    let mut sub_patterns = Vec::new();

    let mut acc = String::new();
    while let Some(c) = pattern_it.next() {
        if c == '(' {
            bracket_stack.push(c);
        }
        if c == ')' {
            bracket_stack.pop();
            if !acc.is_empty() & bracket_stack.is_empty() {
                sub_patterns.push(acc.clone());
                break;
            }
        }
        if c == '|' && !acc.is_empty() && (bracket_stack.len() == 1) {
            sub_patterns.push(acc.clone());
            acc = String::new();
            continue;
        }
        acc.push(c);
    }
    sub_patterns
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_all_subpattern() {
        let pattern = "'(cat) and \\2') is the same as \\1";
        assert_eq!(
            get_all_sub_patterns(&mut pattern.chars()),
            vec!["'(cat) and \\2'"]
        );
        let pattern = "(c.t|d.g) and (f..h|b..d))";
        assert_eq!(
            get_all_sub_patterns(&mut pattern.chars()),
            vec!["(c.t|d.g) and (f..h|b..d)"]
        )
    }

    #[test]
    fn anything_with_capture_group() {
        let query = "((c.t|d.g) and (f..h|b..d)), \\2 with \\3, \\1";
        let content = "\
cat and fish, cat with fish, cat and fish
abc-def is abc-def, not efg, abc, or def
    ";
        assert_eq!(
            vec!["cat and fish, cat with fish, cat and fish"],
            search(content, query)
        );
    }
    #[test]
    fn greedy_neagative_search_with_capture_group() {
        let query = "(([abc]+)-([def]+)) is \\1, not ([^xyz]+), \\2, or \\3";
        let content = "\
grep 101 is doing grep 101 times, and again grep 101 times
abc-def is abc-def, not efg, abc, or def
    ";
        assert_eq!(
            vec!["abc-def is abc-def, not efg, abc, or def"],
            search(content, query)
        );
    }

    #[test]
    fn nested_and_multiple_capture_group() {
        let query = "((\\w\\w\\w\\w) (\\d\\d\\d)) is doing \\2 \\3 times, and again \\1 times";
        let content = "\
grep 101 is doing grep 101 times, and again grep 101 times
3 red squares and 4 red circles
";
        assert_eq!(
            vec!["grep 101 is doing grep 101 times, and again grep 101 times"],
            search(content, query)
        );
    }
    #[test]
    fn multiple_capture_groups() {
        let query = "(\\d+) (\\w+) squares and \\1 \\2 circles";
        let content = "\
3 red squares and 3 red circles
3 red squares and 4 red circles
";
        assert_eq!(
            vec!["3 red squares and 3 red circles"],
            search(content, query)
        );
    }

    #[test]
    fn nested_capture_groups() {
        let query = "('(cat) and \\2') is the same as \\1";
        let content = "\
'cat and cat' is the same as 'cat and cat'
";
        assert_eq!(
            vec!["'cat and cat' is the same as 'cat and cat'"],
            search(content, query)
        );
    }

    #[test]
    fn multiple_capture_groups_with_anchors() {
        let query = "^(apple) (\\w+), \\1 and \\2$";
        let content = "\
pineapple pie, pineapple and pie
apple pie, apple and pie
";
        assert_eq!(vec!["apple pie, apple and pie"], search(content, query));
    }

    #[test]
    fn char_search() {
        let query = "I";
        let content = "\
there is someone
I don't know what to do";
        assert_eq!(vec!["I don't know what to do"], search(content, query));
    }

    #[test]
    fn digit_search() {
        let query = "\\d";
        let content = "\
Full fathom 5 thy father lie
upper hand has lower brises
Follow it till 6 pm today
            ";
        assert_eq!(
            vec!["Full fathom 5 thy father lie", "Follow it till 6 pm today"],
            search(content, query)
        )
    }
    #[test]
    fn char_group_search() {
        let query = "[rnq]";
        let content = "\
Full fathom 5 thq fathe lie
upper hand has lower brises
Follow it till 6 pm today
            ";
        assert_eq!(
            vec!["Full fathom 5 thq fathe lie", "upper hand has lower brises"],
            search(content, query)
        );
    }

    #[test]
    fn neg_char_group_search() {
        let query = "[^rnq]";
        let content = "\
Follow it till 6 pm today
Full fathom 5 thq fathe lie
upper hand has lower brises
";
        assert_eq!(vec!["Follow it till 6 pm today"], search(content, query));
    }

    #[test]
    fn combined_pattern_search() {
        let query = "\\d apple";
        let content = "\
1 apple
2 apples
3 oranges
5 app
";
        assert_eq!(vec!["1 apple", "2 apples"], search(content, query));
    }
    #[test]
    fn string_anchor_start() {
        let query = "^made";
        let content = "\
made in nepal
not made in nepal
";
        assert_eq!(vec!["made in nepal"], search(content, query));
    }
    #[test]
    fn string_achor_end() {
        let query = "made$";
        let content = "\
made in nepal
In nepal made
";
        assert_eq!(vec!["In nepal made"], search(content, query));
    }

    #[test]
    fn one_or_more() {
        let query = "ca+ts";
        let content = "\
my cats
my caaats
";
        assert_eq!(vec!["my cats", "my caaats"], search(content, query));
    }
    #[test]
    fn zero_or_one() {
        let query = "ca?t";
        let content = "\
caat
act
cat
";
        assert_eq!(vec!["act", "cat"], search(content, query));
    }
    #[test]
    fn single_wildcard() {
        let query = "d.g";
        let content = "\
dog
dig
god
";
        assert_eq!(vec!["dog", "dig"], search(content, query));
    }

    #[test]
    fn alternation() {
        let query = "(cat|dog)";
        let content = "\
dog
cat
my cat
my god
";
        assert_eq!(vec!["dog", "cat", "my cat"], search(content, query));
    }

    #[test]
    fn capture_groups() {
        let query = "(\\w+) and \\1";
        let content = "\
cat and cat
dog and dog
cat and dog
";
        assert_eq!(vec!["cat and cat", "dog and dog"], search(content, query));
    }

    #[test]
    fn negative_group_at_end() {
        let query = "([abcd]+) is \\1, not [^xyz]+";
        let content = "\
abcd is abcd, not efg
";
        assert_eq!(vec!["abcd is abcd, not efg"], search(content, query));
    }

    #[test]
    fn capture_group_with_zero_or_more() {
        let query = "once a (drea+mer), alwaysz? a \\1";
        let content = "\
once a dreaaamer, alwayszzz a dreaaamer
";
        assert!(search(content, query).is_empty());
    }
}
