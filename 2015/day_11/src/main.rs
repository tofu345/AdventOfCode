use std::fs;

fn main() {
    let file_contents = match fs::read_to_string("test.txt") {
        Ok(v) => v,
        Err(e) => panic!("{}", e),
    };

    let part_one = next_valid_pass(file_contents.trim());
    let part_two = next_valid_pass(&part_one);
    println!("Part One: {part_one}");
    println!("Part Two: {part_two}");
}

fn is_valid_password(pass: &str) -> bool {
    let pass = pass.as_bytes();
    let mut pairs = Vec::new();
    let mut contains_straight = false;
    for i in 1..pass.len() {
        if is_confusing_letter(pass[i]) {
            return false;
        }

        if pass[i - 1] == pass[i] && !pairs.contains(&pass[i]) {
            pairs.push(pass[i]);
        }

        if i >= 2
            && (pass[i - 2] as i8 - pass[i - 1] as i8) == -1
            && (pass[i - 1] as i8 - pass[i] as i8) == -1
        {
            contains_straight = true;
        }
    }

    pairs.len() >= 2 && contains_straight
}

fn next_valid_pass(pass: &str) -> String {
    let mut bytes = pass.to_string().into_bytes();

    loop {
        for i in (0..bytes.len()).rev() {
            bytes[i] += 1;
            if bytes[i] <= 122 {
                if is_confusing_letter(bytes[i]) {
                    bytes[i] += 1;
                }
                break;
            }

            bytes[i] = 97;
            if i == 0 {
                bytes.insert(0, 97);
            }
        }

        let out = String::from_utf8(bytes).unwrap();
        if is_valid_password(&out) {
            return out;
        }
        bytes = out.into_bytes();
    }
}

fn is_confusing_letter(ch: u8) -> bool {
    ch == 105 || ch == 111 || ch == 108
}
