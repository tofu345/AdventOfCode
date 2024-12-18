use std::cmp::Ordering;
use std::{collections::HashMap, fs};

fn main() {
    let file_contents = match fs::read_to_string("input.txt") {
        Ok(v) => v,
        Err(e) => panic!("{}", e),
    };

    part_one(&file_contents);
}

// i fucking hate regex
//
// Not doing part 2 for the sake of my mental health

fn part_one(file_contents: &str) {
    let mut hands: HashMap<Draw, Vec<Hand>> = HashMap::new();

    for line in file_contents.lines() {
        let split: Vec<&str> = line.split(" ").collect();
        let hand = Hand::from(split[0], split[1]);

        hands
            .entry(Draw::from(split[0]))
            .and_modify(|v| v.push(hand.clone()))
            .or_insert(vec![hand]);
    }

    let mut prev_rank: u32 = 1;
    let mut winnings: u32 = 0;

    for draw_type in TYPES.iter() {
        if let Some(cards) = hands.get(draw_type) {
            rank_hands(&mut winnings, &mut prev_rank, &cards);
        }
    }

    println!("Part One: {winnings}");
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Draw {
    FiveOfAKind,
    FourOfAKind,
    FullHouse,
    ThreeOfAKind,
    TwoPair,
    OnePair,
    HighCard,
}

impl Draw {
    fn from(draw: &str) -> Draw {
        let mut map: HashMap<char, u32> = HashMap::new();
        for ch in draw.chars() {
            map.entry(ch).and_modify(|x| *x += 1).or_insert(1);
        }

        match map.len() {
            1 => Draw::FiveOfAKind,
            2 if map.values().find(|&&v| v == 4).is_some() => Draw::FourOfAKind,
            2 => Draw::FullHouse,
            3 if map.values().find(|&&v| v == 3).is_some() => Draw::ThreeOfAKind,
            3 => Draw::TwoPair,
            4 => Draw::OnePair,
            5 => Draw::HighCard,
            _ => unreachable!("invalid draw"),
        }
    }
}

const CARDS: [char; 13] = [
    'A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2',
];

use Draw::*;
const TYPES: [Draw; 7] = [
    HighCard,
    OnePair,
    TwoPair,
    ThreeOfAKind,
    FullHouse,
    FourOfAKind,
    FiveOfAKind,
];

#[derive(Debug, Clone)]
struct Hand {
    cards: String,
    bid: u32,
}

impl Ord for Hand {
    fn cmp(&self, other: &Self) -> Ordering {
        for (x, y) in self.cards.chars().zip(other.cards.chars()) {
            let strength_x = strength(x);
            let strength_y = strength(y);

            if strength_x > strength_y {
                return Ordering::Greater;
            } else if strength_x < strength_y {
                return Ordering::Less;
            }
        }

        Ordering::Equal
    }
}

impl PartialOrd for Hand {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Hand {
    fn eq(&self, other: &Self) -> bool {
        self.cards == other.cards
    }
}

impl Eq for Hand {}

impl Hand {
    fn from(cards: &str, bid: &str) -> Hand {
        Hand {
            cards: cards.to_owned(),
            bid: bid.parse::<u32>().unwrap(),
        }
    }
}

fn strength(ch: char) -> u32 {
    (CARDS.len() - CARDS.iter().position(|&v| v == ch).expect("invalid card")) as u32
}

fn rank_hands(winnings: &mut u32, prev_rank: &mut u32, hands: &Vec<Hand>) {
    let mut hands = hands.clone();
    hands.sort();

    for hand in hands {
        *winnings += hand.bid * *prev_rank;
        *prev_rank += 1;
    }
}
