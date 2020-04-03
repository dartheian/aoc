use std::env::args_os;
use std::fs::read_to_string;
use std::iter::successors;

fn checked_compute_fuel(mass: i32) -> Option<i32>
{
    if mass >= 9 {Some(mass / 3 - 2)} else {None}
}

fn recursive_compute_fuel(mass: i32) -> i32
{
    successors(checked_compute_fuel(mass),|&m| checked_compute_fuel(m)).sum()
}

fn parse_mass(line: &str) -> i32
{
    line.parse().expect("Error while parsing file")
}

fn main()
{
    let pathname  = args_os().nth(1).expect("Missing input pathname");
    let content   = read_to_string(pathname).expect("Error while reading file");
    let fuel: i32 = content.lines().map(parse_mass).map(recursive_compute_fuel).sum();
    println!("{}", fuel);
}
