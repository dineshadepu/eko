mod binary {
    use eko::Engine;

    #[test]
    fn add() {
        let source = "1 + 1";
        let engine = Engine::new();
        assert_eq!(engine.evaluate_str(source).unwrap(), 2.into());
    }

    #[test]
    fn subtract() {
        let source = "1 - 1";
        let engine = Engine::new();
        assert_eq!(engine.evaluate_str(source).unwrap(), 0.into());
    }

    #[test]
    fn less() {
        let source = "2 < 1";
        let engine = Engine::new();
        assert_eq!(engine.evaluate_str(source).unwrap(), false.into());
    }

    #[test]
    fn greater() {
        let source = "2 > 1";
        let engine = Engine::new();
        assert_eq!(engine.evaluate_str(source).unwrap(), true.into());
    }

    #[test]
    fn and() {
        let source = "true && false";
        let engine = Engine::new();
        assert_eq!(engine.evaluate_str(source).unwrap(), false.into());
    }

    #[test]
    fn or() {
        let source = "true || true";
        let engine = Engine::new();
        assert_eq!(engine.evaluate_str(source).unwrap(), true.into());
    }

    #[test]
    fn compound() {
        let source = "1 + 4 * 5";
        let engine = Engine::new();
        assert_eq!(engine.evaluate_str(source).unwrap(), 21.into());
    }
}

mod unary {
    use eko::Engine;

    #[test]
    fn not() {
        let source = "!false";
        let engine = Engine::new();
        assert_eq!(engine.evaluate_str(source).unwrap(), true.into());
    }

    #[test]
    fn negate() {
        let source = "-10";
        let engine = Engine::new();
        assert_eq!(engine.evaluate_str(source).unwrap(), (-10).into());
    }

    #[test]
    fn compound() {
        let source = "-10 + 33";
        let engine = Engine::new();
        assert_eq!(engine.evaluate_str(source).unwrap(), 23.into());
    }
}

mod var {
    use eko::{Engine, Value};

    #[test]
    fn declaration() {
        let source = "var one";
        let engine = Engine::new();
        assert_eq!(engine.evaluate_str(source).unwrap(), Value::Null);
    }

    #[test]
    fn assignment() {
        let source = "
            var one
            one = 2 * 2
            one
        ";
        let engine = Engine::new();
        assert_eq!(engine.evaluate_str(source).unwrap(), 4.into());
    }

    #[test]
    fn local() {
        let source = "
            var one = 1 + 4
            var two = 2 * 3
            var three = one + two
            three
        ";
        let engine = Engine::new();
        assert_eq!(engine.evaluate_str(source).unwrap(), 11.into());
    }
}

mod func {
    use eko::Engine;

    #[test]
    fn basic() {
        let source = "
            func foo(bar, baz) {}
        ";
        let engine = Engine::new();
        assert!(engine.evaluate_str(source).is_ok());
    }
}

mod r#if {
    use eko::{Engine, Value};

    #[test]
    fn basic() {
        let source = "
            if true && false {
                1 + 1
            }
        ";
        let engine = Engine::new();
        assert_eq!(engine.evaluate_str(source).unwrap(), Value::Null);
    }

    #[test]
    fn with_else() {
        let source = "
            if 39 > 15 {
                21 + 1
            } else {
                10 * 2
            }
        ";
        let engine = Engine::new();
        assert_eq!(engine.evaluate_str(source).unwrap(), 22.into());
    }

    #[test]
    fn with_else_if_and_else() {
        let source = "
            if 39 > 15 {
                21 + 1
            } else if 34 > 22 {
                10 * 2
            } else {
                29 * 3
            }
        ";
        let engine = Engine::new();
        assert_eq!(engine.evaluate_str(source).unwrap(), 22.into());
    }

    #[test]
    fn as_expression() {
        let source = "
            var twenty_two = if 39 > 15 {
                21 + 1
            } else {
                10 * 2
            }
            twenty_two * 2
        ";
        let engine = Engine::new();
        assert_eq!(engine.evaluate_str(source).unwrap(), 44.into());
    }

    #[test]
    fn single_line() {
        let source = "if true { 10 } else { 5 }";
        let engine = Engine::new();
        assert_eq!(engine.evaluate_str(source).unwrap(), 10.into());
    }
}

mod r#while {
    use eko::{Engine, Value};

    #[test]
    fn basic() {
        let source = "
            var x = 10
            var y = 0
            while x > 0 {
                x = x - 1
                y = y + 1
            }
            y
        ";
        let engine = Engine::new();
        assert_eq!(engine.evaluate_str(source).unwrap(), 10.into());
    }

    #[test]
    fn r#break() {
        let source = "
            var x = 10
            while x > 0 {
                x = x - 1
                if x < 5 {
                    break x
                }
            }
        ";
        let engine = Engine::new();
        assert_eq!(engine.evaluate_str(source).unwrap(), 4.into());
    }

    #[test]
    fn no_leaking_scope() {
        let source = "
            var x = 10
            while x > 0 {
                x = x - 1

                var y = 1
                y = y - 1
            }
        ";
        let engine = Engine::new();
        assert_eq!(engine.evaluate_str(source).unwrap(), Value::Null);
    }

    #[test]
    fn single_line() {
        let source = "while false {}";
        let engine = Engine::new();
        assert_eq!(engine.evaluate_str(source).unwrap(), Value::Null);
    }
}

mod try_catch {
    use eko::Engine;

    #[test]
    fn basic() {
        let source = "
            try {
                throw 25
                20
            } catch err {
                err
            }
        ";
        let engine = Engine::new();
        assert_eq!(engine.evaluate_str(source).unwrap(), 25.into());
    }
}

mod newline {
    use eko::Engine;

    #[test]
    fn ok() {
        let source = "
            1 + 4 * 5
            22 / 4.0
        ";
        let engine = Engine::new();
        assert_eq!(engine.evaluate_str(source).unwrap(), 5.5.into());
    }

    #[test]
    fn err() {
        let source = "
            1 + 4 * 5 22 / 4.0
        ";
        let engine = Engine::new();
        assert!(engine.evaluate_str(source).is_err());
    }
}
