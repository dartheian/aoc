use std::error::Error;
use std::env::args_os;
use std::fs::read_to_string;
use std::iter::successors;

fn checked_compute_fuel(&mass: &i32) -> Option<i32>
{
    if mass >= 9 {Some(mass / 3 - 2)} else {None}
}

fn recursive_compute_fuel(mass: &i32) -> i32
{
    successors(checked_compute_fuel(mass), checked_compute_fuel).sum()
}

fn main() -> Result<(), Box<dyn Error>>
{
    let pathname         = args_os().nth(1).ok_or("Missing input pathname")?;
    let content          = read_to_string(pathname)?;
    let masses: Vec<i32> = content.lines().map(|line| line.parse()).collect::<Result<_, _>>()?;
    let fuel: i32        = masses.iter().map(recursive_compute_fuel).sum();
    println!("{}", fuel);
    Ok(())
}