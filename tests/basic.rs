use eko::Engine;

#[test]
fn add() {
    let mut engine = Engine::new();
    assert_eq!(engine.evaluate_expression("1 + 1").unwrap(), 2.into());
}

#[test]
fn subtract() {
    let mut engine = Engine::new();
    assert_eq!(engine.evaluate_expression("1 - 1").unwrap(), 0.into());
}

#[test]
fn less() {
    let mut engine = Engine::new();
    assert_eq!(engine.evaluate_expression("2 < 1").unwrap(), false.into());
}

#[test]
fn greater() {
    let mut engine = Engine::new();
    assert_eq!(engine.evaluate_expression("2 > 1").unwrap(), true.into());
}

#[test]
fn and() {
    let mut engine = Engine::new();
    assert_eq!(
        engine.evaluate_expression("true and false").unwrap(),
        false.into()
    );
}

#[test]
fn or() {
    let mut engine = Engine::new();
    assert_eq!(
        engine.evaluate_expression("true or true").unwrap(),
        true.into()
    );
}

#[test]
fn complex() {
    let mut engine = Engine::new();
    assert_eq!(engine.evaluate_expression("1 + 4 * 5").unwrap(), 21.into());
}