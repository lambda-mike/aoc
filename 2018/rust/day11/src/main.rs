mod day11 {
    use std::collections::HashMap;
    use std::convert::TryInto;

    pub type Power = i32;
    pub type SerialNumber = i32;
    pub type Size = i32;

    #[derive(Clone, Debug, PartialEq, Eq, Hash)]
    pub struct CellSquare {
        pub x: i32,
        pub y: i32,
        pub s: Size,
    }

    type Grid = [[Power; GRID_SIZE]; GRID_SIZE];
    type GridCache = HashMap<CellSquare, Power>;

    const SN: SerialNumber = 7400;
    const GRID_SIZE: usize = 300;

    pub fn solve_a() -> CellSquare {
        let grid = generate_grid(SN);
        find_max_power(grid, Some(3))
    }

    pub fn solve_b() -> CellSquare {
        let grid = generate_grid(SN);
        find_max_power(grid, None)
    }

    pub fn generate_grid(sn: SerialNumber) -> Grid {
        let mut grid: Grid = [[0; GRID_SIZE]; GRID_SIZE];
        for row in 0..GRID_SIZE {
            for col in 0..GRID_SIZE {
                let x: i32 = (col + 1).try_into().unwrap();
                let y: i32 = (row + 1).try_into().unwrap();
                grid[row][col] = calculate_power_level(sn, x, y);
            }
        }
        grid
    }

    pub fn find_max_power(grid: Grid, size: Option<Size>) -> CellSquare {
        // Square size limit
        let grid_size = GRID_SIZE.try_into().unwrap();
        let square_size = size.unwrap_or(grid_size);
        let mut max_power = 0;
        let mut max_power_cell_square = CellSquare { x: 0, y: 0, s: 0 };
        let mut grid_cache: GridCache = HashMap::with_capacity(GRID_SIZE);
        for size in 1..=square_size {
            let max_row = grid_size - size;
            let max_col = max_row;
            for row in 0..max_row {
                for col in 0..max_col {
                    let x = col + 1;
                    let y = row + 1;
                    let cell_square = CellSquare {
                        x: x,
                        y: y,
                        s: size,
                    };
                    let cell_square_power =
                        calculat_square_power(&grid, &mut grid_cache, &cell_square);
                    if cell_square_power > max_power {
                        max_power = cell_square_power;
                        max_power_cell_square = cell_square.clone();
                    }
                    if size >= 2 {
                        // Add square to the cache so we can use it in next loop
                        grid_cache.insert(cell_square.clone(), cell_square_power);
                    }
                    if size >= 3 {
                        // Remove one size lower square as we no longer need it!
                        let square_to_remove = CellSquare {
                            x: x,
                            y: y,
                            s: size - 1,
                        };
                        grid_cache.remove(&square_to_remove);
                    }
                }
            }
        }
        max_power_cell_square
    }

    pub fn calculat_square_power(
        grid: &Grid,
        grid_cache: &mut GridCache,
        cell_square: &CellSquare,
    ) -> Power {
        let row: usize = (cell_square.y - 1).try_into().unwrap();
        let col: usize = (cell_square.x - 1).try_into().unwrap();
        match cell_square.s {
            1 => grid[row][col],
            2 => {
                let top_left = grid[row][col];
                let top_right = grid[row][col + 1];
                let bottom_left = grid[row + 1][col];
                let bottom_right = grid[row + 1][col + 1];
                top_left + top_right + bottom_left + bottom_right
            }
            s if s >= 3 && s <= GRID_SIZE.try_into().unwrap() => {
                let prev_size_square = CellSquare {
                    x: cell_square.x,
                    y: cell_square.y,
                    s: cell_square.s - 1,
                };
                let prev_size_square_power = grid_cache.get(&prev_size_square).unwrap();
                let mut power = *prev_size_square_power;
                // Full last row
                let size: usize = s.try_into().unwrap();
                for c in col..col + size {
                    power += grid[row + size - 1][c];
                }
                // substract 1 for already counted last cell in column
                for r in row..row + size - 1 {
                    power += grid[r][col + size - 1];
                }
                power
            }
            s => panic!("Not supported size: {}!", s),
        }
    }

    pub fn calculate_power_level(sn: SerialNumber, x: i32, y: i32) -> Power {
        let rack_id = x + 10;
        let mut power = rack_id * y;
        power += sn;
        power *= rack_id;
        power = if power < 100 { 0 } else { power / 100 % 10 };
        power -= 5;
        power
    }
}

#[cfg(test)]
mod test_day11 {

    use super::day11::*;

    #[test]
    pub fn test_calculate_power_level1() {
        let sn = 8;
        let x = 3;
        let y = 5;
        assert_eq!(calculate_power_level(sn, x, y), 4);
    }
    #[test]
    pub fn test_calculate_power_level2() {
        let sn = 57;
        let x = 122;
        let y = 79;
        assert_eq!(calculate_power_level(sn, x, y), -5);
    }
    #[test]
    pub fn test_calculate_power_level3() {
        let sn = 39;
        let x = 217;
        let y = 196;
        assert_eq!(calculate_power_level(sn, x, y), 0);
    }
    #[test]
    pub fn test_calculate_power_level4() {
        let sn = 71;
        let x = 101;
        let y = 153;
        assert_eq!(calculate_power_level(sn, x, y), 4);
    }
}

fn main() {
    // 34,72
    println!("Solving Day11A...");
    let result = crate::day11::solve_a();
    println!("{:?}", result);

    // 233,187,13
    println!("Solving Day11B...");
    let result = crate::day11::solve_b();
    println!("{:?}", result);
}
