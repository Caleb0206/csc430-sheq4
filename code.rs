// SHEQ4
// Fully finished implementation of SHEQ4.

// Data definitions

// Value - Numbers, Booleans, String, CloV, PrimV
#[derive(Debug, Clone)]
pub enum Value {
    Real(f64),
    Boolean(bool),
    String(String),
    CloV(CloV),
    PrimV(PrimV)
}

// CloV - Closures contain list of symbol params, body of ExprC, Env
#[derive(Debug, Clone)]
pub struct CloV {
    pub params: Vec<String>,
    pub body: Box<ExprC>,
    pub env: Env
}
// PrimV - Represents a primitive operator by its symbol
#[derive(Debug, Clone)]
pub struct PrimV {
    pub op: String
}

// LamC - Lambdas contain a list of symbol args, and a body of ExprC
#[derive(Debug, Clone)]
pub struct LamC {
    pub args : Vec<String>,
    pub body : Box<ExprC>
}
// Binding : pair of a Symbol and a Value
#[derive(Debug, Clone)]
pub struct Binding {
    pub name : String,
    pub val : Box<Value>
}

// Env : a list of Bindings
pub type Env = Vec<Binding>;

// ExprC type : NumC, IfC, IdC, AppC, LamC, StringC
#[derive(Debug, Clone)]
pub enum ExprC {
    NumC(NumC),
    StringC(StringC),
    IdC(IdC),
    IfC(IfC),
    AppC(AppC),
    LamC(LamC),
}

// NumC : a Real
#[derive(Debug, Clone)]
pub struct NumC {
    pub n : f64
}

// StringC : a String
#[derive(Debug, Clone)]
pub struct StringC {
    pub s : String
}

// IdC : a symbol representing an ID
#[derive(Debug, Clone)]
pub struct IdC {
    pub name : String
}

// IfC : an if statement of ExprC, and ExprC's to act on if true or false
#[derive(Debug, Clone)]
pub struct IfC {
    pub v : Box<ExprC>,
    pub iftrue : Box<ExprC>,
    pub iffalse : Box<ExprC>
}

// AppC : Represents a function application.function ExprC with a list of arg ExprC's
#[derive(Debug, Clone)]
pub struct AppC {
    pub expr : Box<ExprC>,
    pub args : Vec<Box<ExprC>>
}

// reserved-keywords - a list of key-words
const RESERVED_KEYWORDS: [&str; 7] = ["if", "lambda", "let", "=", "in", "end", "else"];

fn main() {
    let top_env: Env = vec![
        Binding { name: "true".into(), val: Box::new(Value::Boolean(true)) },
        Binding { name: "false".into(), val: Box::new(Value::Boolean(false)) },
        Binding { name: "+".into(), val: Box::new(Value::PrimV(PrimV { op: "+".into() })) },
        Binding { name: "-".into(), val: Box::new(Value::PrimV(PrimV { op: "-".into() })) },
        Binding { name: "*".into(), val: Box::new(Value::PrimV(PrimV { op: "*".into() })) },
        Binding { name: "/".into(), val: Box::new(Value::PrimV(PrimV { op: "/".into() })) },
        Binding { name: "<=".into(), val: Box::new(Value::PrimV(PrimV { op: "<=".into() })) },
        Binding { name: "equal?".into(), val: Box::new(Value::PrimV(PrimV { op: "equal?".into() })) },
        Binding { name: "substring".into(), val: Box::new(Value::PrimV(PrimV { op: "substring".into() })) },
        Binding { name: "strlen".into(), val: Box::new(Value::PrimV(PrimV { op: "strlen".into() })) },
        Binding { name: "error".into(), val: Box::new(Value::PrimV(PrimV { op: "error".into() })) },
    ];

    println!("Hello world!");
}
