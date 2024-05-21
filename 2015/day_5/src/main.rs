use std::fs;

fn main() {
    let file_contents = match fs::read_to_string("input.txt") {
        Ok(v) => v,
        Err(_) => panic!("could not read file"),
    };

    if let Err(e) = part_one(&file_contents) {
        eprintln!("Error: {}", e);
        return;
    }

    //  if let Err(e) = part_two(&file_contents) {
    //      eprintln!("error: {}", e);
    //      return;
    //  }
}

fn part_one(file_contents: &str) -> Result<(), &'static str> {
    let mut nice_strings = 0;

    for line in file_contents.lines() {
        let vowels = ['a', 'e', 'i', 'o', 'u'];
        let mut vowel_count = 0;
        let mut previous: Option<char> = None;
        let mut contains_double_letter = false;
        let mut contains_bad_phrase = false;

        for c in line.chars() {
            if let Some(v) = previous {
                if v == c {
                    contains_double_letter = true;
                }

                let phrase = format!("{v}{c}");
                if phrase == "ab" || phrase == "cd" || phrase == "pq" || phrase == "xy" {
                    contains_bad_phrase = true;
                }
            }

            if vowels.contains(&c) {
                vowel_count += 1;
            }

            previous = Some(c);
        }

        if vowel_count >= 3 && contains_double_letter && !contains_bad_phrase {
            nice_strings += 1;
        }
    }

    println!("Nice Strings (part 1): {}", nice_strings);

    Ok(())
}

#[allow(unused)]
fn part_two(file_contents: &str) -> Result<(), &'static str> {
    todo!()
}

