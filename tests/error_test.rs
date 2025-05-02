use c4_rust_Lahbab::error::Error;

#[test]
fn fmt_unexpected_token() {
    let e = Error::UnexpectedToken("foo".into());
    assert_eq!(e.to_string(), "Unexpected token: foo");
}

#[test]
fn fmt_unexpected_eof() {
    let e = Error::UnexpectedEOF;
    assert_eq!(e.to_string(), "Unexpected end of input");
}

#[test]
fn fmt_invalid_syntax() {
    let e = Error::InvalidSyntax("oops".into());
    assert_eq!(e.to_string(), "Invalid syntax: oops");
}

#[test]
fn vm_error_variant() {
    let e = Error::VMError("bad".into());
    assert_eq!(e.to_string(), "VM error: bad");
}
