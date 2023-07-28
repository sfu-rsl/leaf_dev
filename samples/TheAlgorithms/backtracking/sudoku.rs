/* Adapted from: https://github.com/TheAlgorithms/Rust */

include!("../nstd/lib.rs");

fn main() {
    test_sudoku_correct();
}

#[no_mangle]
fn fail() {}

fn are_equal(a: &[[u8; BOARD_SIZE]; BOARD_SIZE], b: &[[u8; BOARD_SIZE]; BOARD_SIZE]) -> bool {
    let mut i = 0;
    while i < a.len() {
        let mut j = 0;
        while j < a[i].len() {
            if a[i][j] != b[i][j] {
                return false;
            }
            j += 1;
        }
        i += 1;
    }

    true
}

fn test_sudoku_correct() {
    let board: [[u8; BOARD_SIZE]; BOARD_SIZE] = [
        [3, 0, 6, 5, 0, 8, 4, 0, 0],
        [5, 2, 0, 0, 0, 0, 0, 0, 0],
        [0, 8, 7, 0, 0, 0, 0, 3, 1],
        [0, 0, 3, 0, 1, 0, 0, 8, 0],
        [9, 0, 0, 8, 6, 3, 0, 0, 5],
        [0, 5, 0, 0, 9, 0, 6, 0, 0],
        [1, 3, 0, 0, 0, 0, 2, 5, 0],
        [0, 0, 0, 0, 0, 0, 0, 7, 4],
        [0, 0, 5, 2, 0, 6, 3, 0, 0],
    ];

    let board_result = [
        [3, 1, 6, 5, 7, 8, 4, 9, 2],
        [5, 2, 9, 1, 3, 4, 7, 6, 8],
        [4, 8, 7, 6, 2, 9, 5, 3, 1],
        [2, 6, 3, 4, 1, 5, 9, 8, 7],
        [9, 7, 4, 8, 6, 3, 1, 2, 5],
        [8, 5, 1, 7, 9, 2, 6, 4, 3],
        [1, 3, 8, 9, 4, 7, 2, 5, 6],
        [6, 9, 2, 3, 5, 1, 8, 7, 4],
        [7, 4, 5, 2, 8, 6, 3, 1, 9],
    ];

    let mut sudoku = Sudoku::new(board);
    let is_solved = sudoku.solve();

    if !is_solved {
        fail();
    }

    if !are_equal(&sudoku.board, &board_result) {
        fail();
    }
}

const BOARD_SIZE: usize = 9;

pub struct Sudoku {
    board: [[u8; BOARD_SIZE]; BOARD_SIZE],
}

impl Sudoku {
    pub fn new(board: [[u8; BOARD_SIZE]; BOARD_SIZE]) -> Sudoku {
        Sudoku { board }
    }

    fn find_empty_cell(&self) -> NSOption<(usize, usize)> {
        let mut i = 0;
        while i < self.board.len() {
            let mut j = 0;
            while j < self.board[i].len() {
                if self.board[i][j] == 0 {
                    return Some((i, j));
                }
                j += 1;
            }
            i += 1;
        }

        None
    }

    fn check(&self, index_tuple: (usize, usize), value: u8) -> bool {
        let (y, x) = index_tuple;

        // checks if the value to be added in the board is an acceptable value for the cell

        // checking through the row
        let mut i = 0;
        while i < BOARD_SIZE {
            if self.board[i][x] == value {
                return false;
            }
            i += 1;
        }
        // checking through the column
        let mut i = 0;
        while i < BOARD_SIZE {
            if self.board[y][i] == value {
                return false;
            }
            i += 1;
        }

        // checking through the 3x3 block of the cell
        let sec_row = y / 3;
        let sec_col = x / 3;

        let mut i = sec_row * 3;
        while i < sec_row * 3 + 3 {
            let mut j = sec_col * 3;
            while j < sec_col * 3 + 3 {
                if self.board[i][j] == value {
                    return false;
                }
                j += 1;
            }
            i += 1;
        }

        true
    }

    pub fn solve(&mut self) -> bool {
        let empty_cell = self.find_empty_cell();

        if let Some((y, x)) = empty_cell {
            let mut val = 1;
            while val < 10 {
                if self.check((y, x), val) {
                    self.board[y][x] = val;
                    if self.solve() {
                        return true;
                    }
                    // backtracking if the board cannot be solved using current configuration
                    self.board[y][x] = 0
                }
                val += 1;
            }
        } else {
            // if the board is complete
            return true;
        }

        // returning false the board cannot be solved using current configuration
        false
    }
}
