use runtime::annotations::Symbolizable;

fn main() {
    let board = get_symbolic_board();
    if !regular::is_valid_sudoku(board) {
        fail();
    }

    if !nsc::is_valid_sudoku(board) {
        fail();
    }
}

fn fail() {}

fn get_symbolic_board() -> [[u8; 9]; 9] {
    let mut board = [[0_u8; 9]; 9];
    let mut i = 0;
    while i < 9 {
        let mut j = 0;
        while j < 9 {
            board[i][j] = (i as u8 + 1).mark_symbolic();
            j += 1;
        }
        i += 1;
    }

    board
}

mod regular {
    pub fn is_valid_sudoku(board: [[u8; 9]; 9]) -> bool {
        // Check range
        let mut i = 0;
        while i < 9 {
            let mut j = 0;
            while j < 9 {
                if board[i][j] < 1 || board[i][j] > 9 {
                    return false;
                }
                j += 1;
            }
            i += 1;
        }

        // Check rows
        let mut i = 0;
        while i < 9 {
            if !is_valid_set(&board[i]) {
                return false;
            }
            i += 1;
        }

        // Check columns
        let mut j = 0;
        while j < 9 {
            let col = {
                let mut col = [0; 9];
                let mut i = 0;
                while i < 9 {
                    col[i] = board[i][j];
                    i += 1;
                }
                col
            };
            if !is_valid_set(&col) {
                return false;
            }
            j += 1;
        }

        // Check 3x3 sub-grids
        let mut i = 0;
        while i < 9 {
            let mut j = 0;
            while j < 9 {
                let subgrid = {
                    let mut subgrid = [0; 9];
                    let mut k = 0;
                    while k < 9 {
                        subgrid[k] = board[i + k / 3][j + k % 3];
                        k += 1;
                    }
                    subgrid
                };
                if !is_valid_set(&subgrid) {
                    return false;
                }
                j += 3;
            }
            i += 3;
        }

        true
    }

    pub fn is_valid_set(nums: &[u8]) -> bool {
        let mut i = 0;
        while i < nums.len() {
            let mut j = i + 1;
            while j < nums.len() {
                if nums[i] == nums[j] {
                    return false;
                }
                j += 1;
            }
            i += 1;
        }

        true
    }
}

// Not short-circuiting
mod nsc {
    pub fn is_valid_sudoku(board: [[u8; 9]; 9]) -> bool {
        let mut result = true;

        // Check range
        let mut i = 0;
        while i < 9 {
            let mut j = 0;
            while j < 9 {
                result = result & (board[i][j] >= 1) & (board[i][j] <= 9);
                j += 1;
            }
            i += 1;
        }

        // Check rows
        let mut i = 0;
        while i < 9 {
            result = result & is_valid_set(&board[i]);
            i += 1;
        }

        // Check columns
        let mut j = 0;
        while j < 9 {
            let col = {
                let mut col = [0; 9];
                let mut i = 0;
                while i < 9 {
                    col[i] = board[i][j];
                    i += 1;
                }
                col
            };
            result = result & is_valid_set(&col);
            j += 1;
        }

        // Check 3x3 sub-grids
        let mut i = 0;
        while i < 9 {
            let mut j = 0;
            while j < 9 {
                let subgrid = {
                    let mut subgrid = [0; 9];
                    let mut k = 0;
                    while k < 9 {
                        subgrid[k] = board[i + k / 3][j + k % 3];
                        k += 1;
                    }
                    subgrid
                };
                result = result & is_valid_set(&subgrid);
                j += 3;
            }
            i += 3;
        }

        result
    }

    pub fn is_valid_set(nums: &[u8]) -> bool {
        let mut result = true;

        let mut i = 0;
        while i < nums.len() {
            let mut j = i + 1;
            while j < nums.len() {
                result = result & (nums[i] != nums[j]);
                j += 1;
            }
            i += 1;
        }

        result
    }
}
