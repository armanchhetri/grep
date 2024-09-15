use std::iter::Peekable;

pub fn search<'a>(content: &'a str, pattern: &str) -> Vec<&'a str> {
    let regex = Regex::compile(pattern);

    // println!("{:?}", regex);
    // println!("{content}, {pattern}");
    let filtered: Vec<&str> = content.lines().filter(|line| regex.execute(line)).collect();
    for line in &filtered {
        println!("{line}");
    }
    filtered
}

#[derive(Debug)]
enum RegexType {
    Invalid,
    Digit,
    Word,
    Char,
    Literal(char),
    CharGroup(String),
}
#[derive(Debug)]
struct Regex {
    tree: Vec<RegexType>,
}

impl Regex {
    fn compile(pattern: &str) -> Regex {
        let mut tree = Vec::new();
        let mut pattern_it = pattern.chars();
        while let Some(c) = pattern_it.next() {
            let reg_type = match c {
                '\\' => match pattern_it.next() {
                    Some('d') => RegexType::Digit,
                    Some('w') => RegexType::Word,
                    Some(_) => RegexType::Char,
                    None => RegexType::Invalid,
                },
                '[' => {
                    let mut char_group = String::new();
                    let mut error = false;
                    loop {
                        match pattern_it.next() {
                            Some(gc) => {
                                if gc == ']' {
                                    break;
                                }
                                char_group.push(gc);
                            }
                            None => {
                                error = true;
                                break;
                            }
                        };
                    }
                    if error {
                        RegexType::Invalid
                    } else {
                        RegexType::CharGroup(char_group)
                    }
                }
                c => RegexType::Literal(c),
            };
            tree.push(reg_type);
        }

        Regex { tree }
    }

    fn execute(&self, line: &str) -> bool {
        let mut line_it = line.chars().peekable();
        for rule in &self.tree {
            if !self.engulf(rule, &mut line_it) {
                return false;
            }
        }
        true
    }

    fn engulf<I>(&self, rule: &RegexType, it: &mut Peekable<I>) -> bool
    where
        I: Iterator<Item = char>,
    {
        while let Some(c) = it.peek() {
            let ret_val = match rule {
                RegexType::Literal(x) => c == x,
                RegexType::Digit => c.is_ascii_digit(),
                RegexType::Word => c.is_ascii_alphabetic(),
                RegexType::CharGroup(char_group) => char_group.contains(*c),
                _ => false,
            };

            it.next();
            if ret_val {
                return ret_val;
            }
        }
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
}
