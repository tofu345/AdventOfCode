use std::fs;

fn main() {
    let file_contents = match fs::read_to_string("test.txt") {
        Ok(v) => v,
        Err(e) => panic!("{}", e),
    };

    look_and_say_x_times(&file_contents, 40);
    look_and_say_x_times(&file_contents, 50);
}

fn look_and_say_x_times(file_contents: &str, x: u32) {
    let mut input = file_contents.trim().to_owned();
    let mut output = String::new();

    for _ in 0..x {
        let mut count = 1;
        let input_bytes = input.as_bytes();
        for i in 1..input.len() {
            if input_bytes[i - 1] != input_bytes[i] {
                output.push_str(&format!("{}{}", count, input_bytes[i - 1] as char));
                count = 0;
            }
            count += 1;
        }
        if count > 0 {
            output.push_str(&format!(
                "{}{}",
                count,
                input_bytes[input.len() - 1] as char
            ));
        }

        input = output;
        output = String::with_capacity(input.len());
    }

    println!("{}", input.len());
}
