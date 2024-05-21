use std::{error::Error, fs};

pub struct Instruction<'a> {
    pub operation: &'a str,
    pub cycles: i32,
    pub value: i32,
}

impl<'a> Instruction<'a> {
    fn build(data: &Vec<&'a str>) -> Result<Vec<Instruction<'a>>, &'static str> {
        let mut instructions: Vec<Instruction> = Vec::new();

        for line in data {
            let words: Vec<&str> = line.split(" ").collect();
            let operation = words[0];

            if operation == "addx" {
                let value = match words[1].parse::<i32>() {
                    Ok(value) => value,
                    Err(_) => return Err("could not parse input"),
                };

                instructions.push(Instruction {
                    operation,
                    cycles: 2,
                    value,
                })
            } else {
                instructions.push(Instruction {
                    operation,
                    cycles: 1,
                    value: 0,
                })
            }
        }

        Ok(instructions)
    }
}

pub fn part_one(filename: &'static str) -> Result<(), Box<dyn Error>> {
    let data = fs::read_to_string(filename)?;
    let data: Vec<&str> = data.lines().collect();

    let mut instructions = Instruction::build(&data)?;

    let mut x: i32 = 1;
    let mut signal_strength = 0;
    let mut i: usize = 0;

    for cycle in 1..221 as i32 {
        if (cycle - 20) % 40 == 0 {
            signal_strength += x * cycle;
        }

        if instructions[i].operation == "addx" {
            instructions[i].cycles -= 1;
            if instructions[i].cycles == 0 {
                x += instructions[i].value;
                i += 1;
            }
        } else if instructions[i].operation == "noop" {
            i += 1;
        } else {
            return Err("unexpected operation".into());
        }
    }

    println!("Signal Strength: {}", signal_strength);

    Ok(())
}

pub fn part_two(filename: &'static str) -> Result<(), Box<dyn Error>> {
    let data = fs::read_to_string(filename)?;
    let data: Vec<&str> = data.lines().collect();

    let mut instructions = Instruction::build(&data)?;

    let mut x: i32 = 1;
    let mut i: usize = 0;

    let mut pixels: Vec<char> = Vec::new();

    for cycle in 1..241 as i32 {
        let mut crt: i32 = cycle;
        if crt > 40 {
            crt = crt % 40;
        }

        if crt == x || crt == x + 1 || crt == x + 2 {
            pixels.push('#');
        } else {
            pixels.push('.')
        }

        if (cycle) % 40 == 0 {
            pixels.push('\n');
        }

        if instructions[i].operation == "addx" {
            instructions[i].cycles -= 1;
            if instructions[i].cycles == 0 {
                x += instructions[i].value;
                i += 1;
            }
        } else if instructions[i].operation == "noop" {
            i += 1;
        } else {
            return Err("unexpected operation".into());
        }
    }

    for c in pixels {
        print!("{c}");
    }

    Ok(())
}
