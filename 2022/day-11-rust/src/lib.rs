use std::{
    collections::{hash_map::Entry, HashMap},
    error::Error,
    fs,
};

#[derive(Debug, Clone)]
struct Monkey {
    items: Vec<u64>,
    operator: String,
    operatee: String,
    divisor: u64,
    monkey_if_true: usize,
    monkey_if_false: usize,
    inspected: usize,
}

impl Monkey {
    fn build(data: &Vec<&str>) -> Result<Vec<Monkey>, Box<dyn Error>> {
        let mut lines: Vec<Vec<&str>> = Vec::new();
        let mut monkey_data: Vec<&str> = Vec::new();

        for index in 0..data.len() {
            monkey_data.push(data[index].trim());

            if (index + 1) % 7 == 0 || index == data.len() - 1 {
                match lines.get(index) {
                    Some(_) => lines[index] = monkey_data.clone(),
                    None => lines.push(monkey_data.clone()),
                }

                monkey_data.clear();
            }
        }

        let mut monkeys: Vec<Monkey> = Vec::new();

        for data in lines {
            if data.len() < 6 {
                return Err("invalid input data".into());
            }

            let item_data = data[1].split(":").collect::<Vec<&str>>()[1];
            let mut items: Vec<u64> = Vec::new();

            for item in item_data.trim().split(", ") {
                items.push(item.parse::<u64>()?)
            }

            let op_data: Vec<&str> = data[2].split("= ").collect::<Vec<&str>>()[1]
                .split(" ")
                .collect();

            let operator = op_data[1].to_string();
            let operatee = op_data[2].to_string();

            let divisor = data[3].split("by ").collect::<Vec<&str>>()[1].parse::<u64>()?;

            let monkey_if_true: usize = data[4].chars().last().unwrap().to_string().parse()?;
            let monkey_if_false: usize = data[5].chars().last().unwrap().to_string().parse()?;

            monkeys.push(Monkey {
                items,
                operator,
                operatee,
                divisor,
                monkey_if_true,
                monkey_if_false,
                inspected: 0,
            })
        }

        Ok(monkeys)
    }

    fn operation(&self, item: u64) -> u64 {
        let value = match self.operatee.as_str() {
            "old" => item,
            num => num.parse::<u64>().unwrap(),
        };

        match self.operator.as_str() {
            "*" => item * value,
            "+" => item + value,
            "-" => item - value,
            "/" => item / value,
            op => panic!("unexpected operator: {}", op),
        }
    }
}

pub fn part_one(filename: &str) -> Result<(), Box<dyn Error>> {
    let data = fs::read_to_string(filename)?;
    let data: Vec<&str> = data.lines().collect();

    let mut monkeys = Monkey::build(&data)?;
    let mut sent_items: HashMap<usize, Vec<u64>> = HashMap::new();

    for _ in 0..20 {
        for i in 0..monkeys.len() {
            let monkey = &monkeys[i];

            for item in &monkey.items {
                let item = monkey.operation(*item) / 3;
                let to_monkey: usize;

                if item % monkey.divisor == 0 {
                    to_monkey = monkey.monkey_if_true;
                } else {
                    to_monkey = monkey.monkey_if_false;
                }

                match sent_items.entry(to_monkey) {
                    Entry::Occupied(mut v) => v.get_mut().push(item),
                    Entry::Vacant(_) => {
                        sent_items.insert(to_monkey, vec![item]);
                        ()
                    }
                };
            }

            monkeys[i].inspected += monkeys[i].items.len();
            monkeys[i].items.clear();

            for (k, values) in &sent_items {
                for v in values {
                    monkeys[*k].items.push(*v);
                }
            }

            sent_items.clear();
        }
    }

    for i in 0..monkeys.len() {
        println!("Monkey {} inspected {} times", i, monkeys[i].inspected);
    }

    Ok(())
}

pub fn part_two(filename: &str) -> Result<(), Box<dyn Error>> {
    todo!();
}
