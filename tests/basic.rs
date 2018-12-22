use eko::{Engine, Value};

#[test]
fn add() {
    let mut engine = Engine::new();
    assert_eq!(engine.evaluate("1 + 1").unwrap(), 2.into());
}

#[test]
fn subtract() {
    let mut engine = Engine::new();
    assert_eq!(engine.evaluate("1 - 1").unwrap(), 0.into());
}

#[test]
fn less() {
    let mut engine = Engine::new();
    assert_eq!(engine.evaluate("2 < 1").unwrap(), false.into());
}

#[test]
fn greater() {
    let mut engine = Engine::new();
    assert_eq!(engine.evaluate("2 > 1").unwrap(), true.into());
}

#[test]
fn and() {
    let mut engine = Engine::new();
    assert_eq!(engine.evaluate("true and false").unwrap(), false.into());
}

#[test]
fn or() {
    let mut engine = Engine::new();
    assert_eq!(engine.evaluate("true or true").unwrap(), true.into());
}

#[test]
fn complex() {
    let mut engine = Engine::new();
    assert_eq!(engine.evaluate("1 + 4 * 5").unwrap(), 21.into());
}

#[test]
fn newline() {
    let source = "
        1 + 4 * 5
        22 / 4.0
    ";
    let mut engine = Engine::new();
    assert_eq!(engine.evaluate(source).unwrap(), 5.5.into());
}

#[test]
fn assignment() {
    let source = "
        var one
        one = 2 * 2
        one
    ";
    let mut engine = Engine::new();
    assert_eq!(engine.evaluate(source).unwrap(), 4.into());
}

#[test]
fn local() {
    let source = "
        var one = 1 + 4
        var two = 2 * 3
        var three = one + two
        three
    ";
    let mut engine = Engine::new();
    assert_eq!(engine.evaluate(source).unwrap(), 11.into());
}

#[test]
fn r#if() {
    let source = "
        if true and false {
            1 + 1
        }
    ";
    let mut engine = Engine::new();
    assert_eq!(engine.evaluate(source).unwrap(), Value::Null);
}

#[test]
fn if_else() {
    let source = "
        if 39 > 15 {
            21 + 1
        } else {
            10 * 2
        }
    ";
    let mut engine = Engine::new();
    assert_eq!(engine.evaluate(source).unwrap(), 22.into());
}

#[test]
fn if_else_if_else() {
    let source = "
        if 39 > 15 {
            21 + 1
        } else if 34 > 22 {
            10 * 2
        } else {
            29 * 3
        }
    ";
    let mut engine = Engine::new();
    assert_eq!(engine.evaluate(source).unwrap(), 22.into());
}
