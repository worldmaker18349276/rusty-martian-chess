#[allow(unused)]

mod model {
    use std::fmt;
    use std::ops::{Add, Mul};
    use std::vec::Vec;

    #[derive(Debug, Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
    pub enum Chess {
        Pawn,
        Drone,
        Queen,
    }

    #[derive(Debug, Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
    pub enum Player {
        Player1,
        Player2,
    }

    #[derive(Debug, Copy, Clone, Eq, PartialEq)]
    pub struct Playfield {
        board: [[Option<Chess>; 4]; 8],
        scores: [i32; 2],
        turn: Player,
    }

    #[derive(Debug, Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
    pub struct Point(pub i32, pub i32);

    impl Add<Point> for Point {
        type Output = Point;

        fn add(self, other: Point) -> Point {
            Point(self.0 + other.0, self.1 + other.1)
        }
    }

    impl Mul<i32> for Point {
        type Output = Point;

        fn mul(self, other: i32) -> Point {
            Point(self.0 * other, self.1 * other)
        }
    }

    #[derive(Debug, Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
    pub enum Movement {
        PawnUpLeft,
        PawnUpRight,
        PawnDownLeft,
        PawnDownRight,

        DroneUp,
        DroneUp2,
        DroneDown,
        DroneDown2,
        DroneLeft,
        DroneLeft2,
        DroneRight,
        DroneRight2,

        QueenUp(usize),
        QueenDown(usize),
        QueenLeft(usize),
        QueenRight(usize),
        QueenUpLeft(usize),
        QueenUpRight(usize),
        QueenDownLeft(usize),
        QueenDownRight(usize),
    }

    impl Movement {
        fn value(&self) -> (Chess, Point, usize) {
            match self {
                Movement::PawnUpLeft    => (Chess::Pawn, Point(-1,-1), 0),
                Movement::PawnUpRight   => (Chess::Pawn, Point(-1, 1), 0),
                Movement::PawnDownLeft  => (Chess::Pawn, Point( 1,-1), 0),
                Movement::PawnDownRight => (Chess::Pawn, Point( 1, 1), 0),
        
                Movement::DroneUp     => (Chess::Drone, Point(-1, 0), 0),
                Movement::DroneUp2    => (Chess::Drone, Point(-1, 0), 1),
                Movement::DroneDown   => (Chess::Drone, Point( 1, 0), 0),
                Movement::DroneDown2  => (Chess::Drone, Point( 1, 0), 1),
                Movement::DroneLeft   => (Chess::Drone, Point( 0,-1), 0),
                Movement::DroneLeft2  => (Chess::Drone, Point( 0,-1), 1),
                Movement::DroneRight  => (Chess::Drone, Point( 0, 1), 0),
                Movement::DroneRight2 => (Chess::Drone, Point( 0, 1), 1),
        
                Movement::QueenUp(cross)        => (Chess::Queen, Point(-1, 0), *cross),
                Movement::QueenDown(cross)      => (Chess::Queen, Point( 1, 0), *cross),
                Movement::QueenLeft(cross)      => (Chess::Queen, Point( 0,-1), *cross),
                Movement::QueenRight(cross)     => (Chess::Queen, Point( 0, 1), *cross),
                Movement::QueenUpLeft(cross)    => (Chess::Queen, Point(-1,-1), *cross),
                Movement::QueenUpRight(cross)   => (Chess::Queen, Point(-1, 1), *cross),
                Movement::QueenDownLeft(cross)  => (Chess::Queen, Point( 1,-1), *cross),
                Movement::QueenDownRight(cross) => (Chess::Queen, Point( 1, 1), *cross),
            }
        }

        fn possible_movements_of(chess: &Chess) -> Vec<Self> {
            match chess {
                Chess::Pawn => vec![
                    Movement::PawnUpLeft,
                    Movement::PawnUpRight,
                    Movement::PawnDownLeft,
                    Movement::PawnDownRight,
                ],
                Chess::Drone => vec![
                    Movement::DroneUp,
                    Movement::DroneUp2,
                    Movement::DroneDown,
                    Movement::DroneDown2,
                    Movement::DroneLeft,
                    Movement::DroneLeft2,
                    Movement::DroneRight,
                    Movement::DroneRight2,
                ],
                Chess::Queen => {
                    let mut res = Vec::new();
                    for cross in 0..8 {
                        res.push(Movement::QueenUp(cross));
                        res.push(Movement::QueenDown(cross));
                        res.push(Movement::QueenLeft(cross));
                        res.push(Movement::QueenRight(cross));
                        res.push(Movement::QueenUpLeft(cross));
                        res.push(Movement::QueenUpRight(cross));
                        res.push(Movement::QueenDownLeft(cross));
                        res.push(Movement::QueenDownRight(cross));
                    }
                    res
                },
            }
        }
    }

    #[derive(Debug, Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
    pub enum Effect {
        Move,
        Capture(i32),
        // Promotion,
    }

    #[derive(Debug, Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
    pub struct Action {
        player: Player,
        position: Point,
        movement: Movement,
        effect: Effect,
    }

    #[derive(Debug, Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
    pub struct OutOfBounds(Point);

    #[derive(Debug, Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
    pub struct InvalidAction(Action);
    
    impl Chess {
        fn point(&self) -> i32 {
            match self {
                Chess::Pawn => 1,
                Chess::Drone => 2,
                Chess::Queen => 3,
            }
        }
    }

    impl Player {
        fn index(&self) -> usize {
            match self {
                Player::Player1 => 0,
                Player::Player2 => 1,
            }
        }

        fn next(&self) -> Player {
            match self {
                Player::Player1 => Player::Player2,
                Player::Player2 => Player::Player1,
            }
        }
    }

    impl Playfield {
        pub fn init() -> Playfield {
            let p = Option::Some(Chess::Pawn);
            let d = Option::Some(Chess::Drone);
            let q = Option::Some(Chess::Queen);
            let n = Option::<Chess>::None;

            Playfield {
                board: [
                    [q, q, d, n],
                    [q, d, p, n],
                    [d, p, p, n],
                    [n, n, n, n],
                    [n, n, n, n],
                    [n, p, p, d],
                    [n, p, d, q],
                    [n, d, q, q],
                ],
                scores: [0, 0],
                turn: Player::Player1,
            }
        }

        pub fn get_board(&self) -> &[[Option<Chess>; 4]; 8] {
            &self.board
        }

        pub fn get_score(&self, player: Player) -> &i32 {
            &self.scores[player.index()]
        }

        pub fn get_turn(&self) -> &Player {
            &self.turn
        }

        pub fn get_chess(&self, position: &Point) -> Result<(Player, Option<Chess>), OutOfBounds> {
            if position.0 >= 0 && position.0 < 4 && position.1 >= 0 && position.1 < 4 {
                Ok((Player::Player1, self.board[position.0 as usize][position.1 as usize]))
            } else if position.0 >= 4 && position.0 < 8 && position.1 >= 0 && position.1 < 4 {
                Ok((Player::Player2, self.board[position.0 as usize][position.1 as usize]))
            } else {
                Err(OutOfBounds(*position))
            }
        }

        pub fn get_effect(&self, position: &Point, movement: &Movement) -> Option<Effect> {
            let (chess, dir, cross) = movement.value();
            if let Ok((player, Option::Some(chess_))) = self.get_chess(position) {
                if chess_ != chess {
                    return Option::None;
                }

                // TODO: no rejection
                
                let mut goal = *position;
                for _ in 0..cross {
                    goal = goal + dir;
                    if let Ok((_, Option::None)) = self.get_chess(&goal) {
                        continue;
                    } else {
                        return Option::None;
                    }
                }

                goal = goal + dir;
                match self.get_chess(&goal) {
                    Err(_) => Option::None,
                    Ok((_, Option::None)) => Option::Some(Effect::Move),
                    Ok((owner, Option::Some(chess))) => {
                        if owner != player {
                            return Option::Some(Effect::Capture(chess.point()));
                        } else {
                            // TODO: field promotion
                            return Option::None;
                        }
                    },
                }
            } else {
                return Option::None;
            }
        }

        pub fn possible_actions(&self, position: &Point) -> Vec<Action> {
            let mut vec = Vec::<Action>::new();

            if let Ok((player, Option::Some(chess))) = self.get_chess(position) {
                for movement in Movement::possible_movements_of(&chess) {
                    if let Option::Some(effect) = self.get_effect(position, &movement) {
                        vec.push(Action { player, position: *position, movement, effect });
                    }
                }
                return vec;
            } else {
                return vec;
            }
        }

        pub fn is_valid_action(&self, action: &Action) -> bool {
            action.player == self.turn && self.get_effect(&action.position, &action.movement) == Option::Some(action.effect)
        }

        pub fn apply_action(&mut self, action: &Action) -> Result<(), InvalidAction> {
            if !self.is_valid_action(action) {
                return Err(InvalidAction(*action));
            }

            let start = action.position;
            let (chess, dir, cross) = action.movement.value();
            // assert_eq!(self.board[start.0 as usize][start.1 as usize], chess);
            let goal = start + dir * ((cross + 1) as i32);

            match action.effect {
                Effect::Move => {
                    self.board[start.0 as usize][start.1 as usize] = Option::None;
                    self.board[goal.0 as usize][goal.1 as usize] = Option::Some(chess);
                },
                Effect::Capture(point) => {
                    self.board[start.0 as usize][start.1 as usize] = Option::None;
                    self.board[goal.0 as usize][goal.1 as usize] = Option::Some(chess);
                    self.scores[self.turn.index()] += point;
                },
                // TODO: field promotion
            }
            Ok(())
        }
    }

    impl fmt::Display for Playfield {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "{}", self.scores[0])?;
            match self.turn {
                Player::Player1 => write!(f, "*\n")?,
                Player::Player2 => write!(f, "\n")?,
            }

            for line in self.board.iter() {
                for grid in line.iter() {
                    match grid {
                        Option::None => write!(f, ".")?,
                        Option::Some(Chess::Pawn) => write!(f, "p")?,
                        Option::Some(Chess::Drone) => write!(f, "d")?,
                        Option::Some(Chess::Queen) => write!(f, "q")?,
                    }
                    write!(f, " ")?;
                }
                write!(f, "\n")?;
            }

            write!(f, "{}", self.scores[1])?;
            match self.turn {
                Player::Player1 => write!(f, "\n")?,
                Player::Player2 => write!(f, "*\n")?,
            }

            Ok(())
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn point_add() {
            assert_eq!(Point(1, 2) + Point(3, 4), Point(4, 6));
            assert_eq!(Point(1, -2) + Point(-3, 4), Point(-2, 2));
        }

        #[test]
        fn playfield_get_owner() {
            let playfield = Playfield::init();

            for x in 0..4 {
                for y in 0..4 {
                    assert!(
                        match playfield.get_chess(&Point(x, y)) {
                            Ok((Player::Player1, _)) => true,
                            _ => false,
                        }
                    );
                }
            }

            for x in 4..8 {
                for y in 0..4 {
                    assert!(
                        match playfield.get_chess(&Point(x, y)) {
                            Ok((Player::Player2, _)) => true,
                            _ => false,
                        }
                    );
                }
            }
        }

        #[test]
        fn playfield_get_out_of_bounds() {
            let playfield = Playfield::init();

            assert_eq!(playfield.get_chess(&Point(-1, 3)), Err(OutOfBounds(Point(-1, 3))));
            assert_eq!(playfield.get_chess(&Point(9, 3)), Err(OutOfBounds(Point(9, 3))));
            assert_eq!(playfield.get_chess(&Point(2, -2)), Err(OutOfBounds(Point(2, -2))));
            assert_eq!(playfield.get_chess(&Point(2, 4)), Err(OutOfBounds(Point(2, 4))));
        }

        #[test]
        fn playfield_possible_actions_invalid() {
            let p = Option::Some(Chess::Pawn);
            let d = Option::Some(Chess::Drone);
            let q = Option::Some(Chess::Queen);
            let n = Option::<Chess>::None;

            let playfield = Playfield {
                board: [
                    [n, n, n, n],
                    [n, n, n, n],
                    [n, n, n, p],
                    [n, n, n, n],
                    [n, n, n, n],
                    [n, n, n, n],
                    [n, n, n, p],
                    [n, n, n, n],
                ],
                scores: [0, 0],
                turn: Player::Player1,
            };

            let playfield2 = Playfield {
                board: [
                    [n, n, n, n],
                    [n, n, n, n],
                    [n, n, n, p],
                    [n, n, n, n],
                    [n, n, n, n],
                    [n, n, n, n],
                    [n, n, n, p],
                    [n, n, n, n],
                ],
                scores: [0, 0],
                turn: Player::Player2,
            };

            assert_eq!(playfield.possible_actions(&Point(3, 2)), vec![]); // empty grid
            assert_eq!(playfield.possible_actions(&Point(2, 5)), vec![]); // out of bounds
            assert_ne!(playfield.possible_actions(&Point(6, 3)), vec![]); // not restricted by turn
            assert_ne!(playfield2.possible_actions(&Point(2, 3)), vec![]); // not restricted by turn
        }

        #[test]
        fn playfield_possible_actions_pawn() {
            let p = Option::Some(Chess::Pawn);
            let d = Option::Some(Chess::Drone);
            let q = Option::Some(Chess::Queen);
            let n = Option::<Chess>::None;

            let playfield = Playfield {
                board: [
                    [n, n, n, n],
                    [n, n, n, n],
                    [n, p, n, n],
                    [n, n, p, n],
                    [n, d, n, n],
                    [n, n, n, n],
                    [n, n, n, n],
                    [n, n, n, n],
                ],
                scores: [0, 0],
                turn: Player::Player1,
            };
            let mut moves = playfield.possible_actions(&Point(3, 2));
            let mut expected = vec![
                Action {player: Player::Player1, position: Point(3, 2), movement: Movement::PawnUpRight, effect: Effect::Move},
                // cross canal
                Action {player: Player::Player1, position: Point(3, 2), movement: Movement::PawnDownRight, effect: Effect::Move},
                // // stop by chess
                // Action {player: Player::Player1, position: Point(3, 2), movement: Movement::PawnUpLeft, effect: Effect::Move},
                // capture chess in the enemy's territory
                Action {player: Player::Player1, position: Point(3, 2), movement: Movement::PawnDownLeft, effect: Effect::Capture(2)},
            ];
            moves.sort();
            expected.sort();
            assert_eq!(moves, expected);
        }

        #[test]
        fn playfield_possible_actions_drone() {
            let p = Option::Some(Chess::Pawn);
            let d = Option::Some(Chess::Drone);
            let q = Option::Some(Chess::Queen);
            let n = Option::<Chess>::None;

            let playfield = Playfield {
                board: [
                    [n, n, n, n],
                    [n, n, n, n],
                    [n, n, n, n],
                    [n, p, d, n],
                    [n, n, n, n],
                    [n, n, q, n],
                    [n, n, n, n],
                    [n, n, n, n],
                ],
                scores: [0, 0],
                turn: Player::Player1,
            };
            let mut moves = playfield.possible_actions(&Point(3, 2));
            let mut expected = vec![
                Action {player: Player::Player1, position: Point(3, 2), movement: Movement::DroneUp, effect: Effect::Move},
                // cross an empty grid
                Action {player: Player::Player1, position: Point(3, 2), movement: Movement::DroneUp2, effect: Effect::Move},
                Action {player: Player::Player1, position: Point(3, 2), movement: Movement::DroneRight, effect: Effect::Move},
                // // stop by boundary
                // Action {player: Player::Player1, position: Point(3, 2), movement: Movement::DroneRight2, effect: Effect::Move},
                // // stop by chess
                // Action {player: Player::Player1, position: Point(3, 2), movement: Movement::DroneLeft, effect: Effect::Move},
                // Action {player: Player::Player1, position: Point(3, 2), movement: Movement::DroneLeft2, effect: Effect::Move},
                // cross canal
                Action {player: Player::Player1, position: Point(3, 2), movement: Movement::DroneDown, effect: Effect::Move},
                // capture chess in the enemy's territory
                Action {player: Player::Player1, position: Point(3, 2), movement: Movement::DroneDown2, effect: Effect::Capture(3)},
            ];
            moves.sort();
            expected.sort();
            assert_eq!(moves, expected);
        }

        #[test]
        fn playfield_possible_actions_queen() {
            let p = Option::Some(Chess::Pawn);
            let d = Option::Some(Chess::Drone);
            let q = Option::Some(Chess::Queen);
            let n = Option::<Chess>::None;

            let playfield = Playfield {
                board: [
                    [n, n, n, n],
                    [n, n, n, n],
                    [n, n, n, n],
                    [p, n, q, n],
                    [n, n, n, n],
                    [p, n, n, n],
                    [n, n, n, n],
                    [n, n, d, n],
                ],
                scores: [0, 0],
                turn: Player::Player1,
            };
            let mut moves = playfield.possible_actions(&Point(3, 2));
            let mut expected = vec![
                Action {player: Player::Player1, position: Point(3, 2), movement: Movement::QueenUp(0), effect: Effect::Move},
                // cross an empty grid
                Action {player: Player::Player1, position: Point(3, 2), movement: Movement::QueenUp(1), effect: Effect::Move},
                Action {player: Player::Player1, position: Point(3, 2), movement: Movement::QueenUp(2), effect: Effect::Move},
                Action {player: Player::Player1, position: Point(3, 2), movement: Movement::QueenRight(0), effect: Effect::Move},
                // // stop by boundary
                // Action {player: Player::Player1, position: Point(3, 2), movement: Movement::QueenRight(1), effect: Effect::Move},
                Action {player: Player::Player1, position: Point(3, 2), movement: Movement::QueenLeft(0), effect: Effect::Move},
                // // stop by chess
                // Action {player: Player::Player1, position: Point(3, 2), movement: Movement::QueenRight(1), effect: Effect::Move},
                // cross canal
                Action {player: Player::Player1, position: Point(3, 2), movement: Movement::QueenDown(0), effect: Effect::Move},
                Action {player: Player::Player1, position: Point(3, 2), movement: Movement::QueenDown(1), effect: Effect::Move},
                Action {player: Player::Player1, position: Point(3, 2), movement: Movement::QueenDown(2), effect: Effect::Move},
                // capture chess in the enemy's territory
                Action {player: Player::Player1, position: Point(3, 2), movement: Movement::QueenDown(3), effect: Effect::Capture(2)},

                // diagonal moves
                Action {player: Player::Player1, position: Point(3, 2), movement: Movement::QueenUpLeft(0), effect: Effect::Move},
                Action {player: Player::Player1, position: Point(3, 2), movement: Movement::QueenUpLeft(1), effect: Effect::Move},
                Action {player: Player::Player1, position: Point(3, 2), movement: Movement::QueenUpRight(0), effect: Effect::Move},
                Action {player: Player::Player1, position: Point(3, 2), movement: Movement::QueenDownRight(0), effect: Effect::Move},
                Action {player: Player::Player1, position: Point(3, 2), movement: Movement::QueenDownLeft(0), effect: Effect::Move},
                // diagonal capture
                Action {player: Player::Player1, position: Point(3, 2), movement: Movement::QueenDownLeft(1), effect: Effect::Capture(1)},
            ];
            moves.sort();
            expected.sort();
            assert_eq!(moves, expected);
        }
    }
}

mod tui {
    extern crate pancurses;
    use super::model;

    pub struct Board<'a> {
        grids: &'a [[Option<model::Chess>; 4]; 8],
        score1: &'a i32,
        score2: &'a i32,
        turn: &'a model::Player,
        cursor: model::Point,
        highlighted: Vec<model::Point>,
        bracketed: Vec<model::Point>,
    }
    pub fn draw_board(window: &pancurses::Window, board: &Board) {
        window.clear();

        for y in 0..8 {
            for x in 0..4 {
                let sym = match board.grids[y][x] {
                    Option::Some(model::Chess::Pawn) => "*",
                    Option::Some(model::Chess::Drone) => "o",
                    Option::Some(model::Chess::Queen) => "@",
                    Option::None => ".",
                };
                window.mv(y as i32, x as i32 * 2 + 1);
                if board.highlighted.contains(&model::Point(y as i32, x as i32)) {
                    window.attron(pancurses::A_BOLD);
                } else {
                    window.attroff(pancurses::A_BOLD);
                }
                if board.bracketed.contains(&model::Point(y as i32, x as i32)) {
                    window.attron(pancurses::A_UNDERLINE);
                } else {
                    window.attroff(pancurses::A_UNDERLINE);
                }
                window.addstr(sym);

                if y as i32 == board.cursor.0 && x as i32 == board.cursor.1 {
                    window.mv(y as i32, x as i32 * 2);
                    window.addstr("[");
                    window.mv(y as i32, x as i32 * 2 + 2);
                    window.addstr("]");
                }
            }
        }

        window.mv(0, 12);
        if let model::Player::Player1 = board.turn {
            window.addstr(format!("<{}>", board.score1));
        } else {
            window.addstr(format!(" {} ", board.score1));
        }

        window.mv(7, 12);
        if let model::Player::Player1 = board.turn {
            window.addstr(format!(" {} ", board.score2));
        } else {
            window.addstr(format!("<{}>", board.score2));
        }
    }
}

fn main() {
    let playfield = model::Playfield::init();
    println!("{}", playfield);
}
