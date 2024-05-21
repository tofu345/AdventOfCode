use std::fs;

fn main() {
    let file_contents = match fs::read_to_string("input.txt") {
        Ok(v) => v,
        Err(e) => panic!("{}", e),
    };

    part_one(&file_contents);
    part_two(&file_contents);
}

fn part_one(file_contents: &str) {
    println!("Part One: {}", sum(file_contents));
}

fn part_two(file_contents: &str) {
    let data = remove_red(file_contents.to_owned());
    println!("Part Two: {}", sum(&data));
}

fn sum(data: &str) -> i32 {
    let mut sum = 0;
    let mut num = String::new();
    for ch in data.chars() {
        if ch.is_ascii_digit() || ch == '-' {
            num.push(ch)
        } else if !num.is_empty() {
            sum += num.parse::<i32>().unwrap();
            num.clear();
        }
    }

    sum
}

fn remove_red(data: String) -> String {
    if data.len() < 3 {
        return data;
    }

    let data_bytes = data.as_bytes();
    for i in 2..data.len() {
        if !is_red(data_bytes, i) {
            continue;
        }

        let (start, end) = find_start_and_end(data_bytes, i);
        return remove_red(
            data.chars()
                .take(start)
                .chain(data.chars().skip(end + 1))
                .collect(),
        );
    }

    data
}

fn is_red(arr: &[u8], i: usize) -> bool {
    arr[i - 2] == b'r' && arr[i - 1] == b'e' && arr[i] == b'd'
}

fn find_start_and_end(data: &[u8], i: usize) -> (usize, usize) {
    let mut num_nested = 0;
    for j in (0..=i).rev() {
        match data[j] {
            b'}' | b']' => num_nested += 1,
            b'{' | b'[' => num_nested -= 1,
            _ => continue,
        }

        if num_nested < 0 {
            if data[j] == b'{' {
                return (j, find_end_of_obj(data, i));
            }
            break;
        }
    }

    (i - 2, i)
}

fn find_end_of_obj(data: &[u8], i: usize) -> usize {
    let mut nested_objs = 0;
    for (j, &ch) in data.iter().enumerate().skip(i) {
        match data[j] {
            b'}' => nested_objs -= 1,
            b'{' => nested_objs += 1,
            _ => continue,
        }

        if nested_objs < 0 && ch == b'}' {
            return j;
        }
    }

    data.len()
}
