use fancy_regex::Regex;
use std::fs;

fn main() -> std::io::Result<()> {
    let file_contents = fs::read_to_string("input.txt")?;
    part_one(&file_contents);
    part_two(&file_contents);
    Ok(())
}

fn part_one(file_contents: &str) {
    let re = Regex::new(r"(.)\1").unwrap();
    let re1 = Regex::new(r"^((?!ab|cd|pq|xy).)*$").unwrap();
    let nice_strings = file_contents
        .lines()
        .filter(|&line| {
            re.is_match(line).is_ok_and(|v| v)
                && re1.is_match(line).is_ok_and(|v| v)
                && line
                    .chars()
                    .filter(|c| ['a', 'e', 'i', 'o', 'u'].contains(c))
                    .count()
                    >= 3
        })
        .collect::<Vec<&str>>();
    println!("Nice Strings (part 1): {}", nice_strings.len());
}

fn part_two(file_contents: &str) {
    let re = Regex::new(r"^.*?([a-z]{2}).*?(\1).*$").unwrap();
    let re1 = Regex::new(r"(.).\1").unwrap();
    let nice_strings = file_contents
        .lines()
        .filter(|&line| re.is_match(line).is_ok_and(|v| v) && re1.is_match(line).is_ok_and(|v| v))
        .collect::<Vec<&str>>();
    println!("Nice Strings (part 2): {}", nice_strings.len());
}
