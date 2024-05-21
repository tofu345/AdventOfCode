use std::fs;

fn main() {
    let file_contents = match fs::read_to_string("input.txt") {
        Ok(v) => v,
        Err(e) => panic!("{}", e),
    };

    if let Err(e) = part_one(&file_contents) {
        eprintln!("Error: {}", e);
        return;
    }

    // if let Err(e) = part_two(filename) {
    //     eprintln!("Error: {}", e);
    //     return;
    // }
}

fn part_one(file_contents: &str) -> Result<(), &'static str> {
    let mut num = 0;

    for line in file_contents.lines() {
        let line: Vec<char> = line.chars().collect();
        let code_len = line.len();
        let mut mem_len = code_len - 2;

        for (i, ch) in line.iter().enumerate() {
            if *ch == '\\' {
                match line[(i + 1) as usize] {
                    'x' => mem_len -= 3,
                    _ => mem_len -= 1,
                };
            };
        }

        num += code_len - mem_len;
    }

    println!("Part One: {}", num);

    Ok(())
}
