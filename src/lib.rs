use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub enum Operator {
    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
    LessEquals,
    GreaterEquals,
    Contains,
    NotContains,
    Matches,
}

#[derive(Debug, PartialEq)]
pub enum LogicalOp {
    And,
    Or,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Comparaison {
        field: String,
        op: Operator,
        value: String,
    },
    Logical {
        left: Box<Expression>,
        op: LogicalOp,
        right: Box<Expression>,
    },
    Group(Box<Expression>),
}

impl Expression {
    pub fn create_param(value: &str, params: &mut Vec<String>) -> String {
        params.push(value.to_string());
        format!("${}", params.len())
    }

    pub fn evaluate(&self, record: &HashMap<String, String>) -> bool {
        match self {
            Expression::Comparaison { field, op, value } => {
                let empty = "".to_string();

                if field.to_lowercase() == "any" {
                    return record.values().any(|v| match op {
                        Operator::Equals => v == value,
                        Operator::NotEquals => v != value,
                        Operator::Contains => v.contains(value),
                        Operator::NotContains => !v.contains(value),
                        Operator::Matches => {
                            let re = regex::Regex::new(value).unwrap();
                            re.is_match(v)
                        }
                        _ => false,
                    });
                }

                let record_value = record.get(field).unwrap_or(&empty);
                match op {
                    Operator::Equals => record_value == value,
                    Operator::NotEquals => record_value != value,
                    Operator::Contains => record_value.contains(value),
                    Operator::NotContains => !record_value.contains(value),
                    Operator::Matches => {
                        let re = regex::Regex::new(value).unwrap();
                        re.is_match(record_value)
                    }
                    _ => false,
                }
            }
            Expression::Logical { left, op, right } => match op {
                LogicalOp::And => left.evaluate(record) && right.evaluate(record),
                LogicalOp::Or => left.evaluate(record) || right.evaluate(record),
            },
            Expression::Group(expr) => expr.evaluate(record),
        }
    }
}

#[derive(Clone, Debug)]
pub struct SqlOptions {
    pub columns: HashMap<String, String>,
    pub file_prefix: Option<String>,
}

impl Default for SqlOptions {
    fn default() -> Self {
        let mut columns = HashMap::new();
        columns.insert("Album".to_string(), "album".to_string());
        columns.insert("Artist".to_string(), "artist".to_string());
        columns.insert("Title".to_string(), "title".to_string());
        columns.insert("File".to_string(), "file".to_string());
        SqlOptions {
            columns,
            file_prefix: None,
        }
    }
}

pub trait ToSql {
    fn to_sql(&self, options: SqlOptions) -> (String, Vec<String>);
    fn to_sql_internal(&self, params: &mut Vec<String>, options: SqlOptions) -> String;
}

impl ToSql for Expression {
    fn to_sql(&self, options: SqlOptions) -> (String, Vec<String>) {
        let mut params = Vec::new();
        let query = self.to_sql_internal(&mut params, options);
        (query, params)
    }
    fn to_sql_internal(&self, params: &mut Vec<String>, options: SqlOptions) -> String {
        match self {
            Expression::Comparaison { field, op, value } => {
                let op_str = match op {
                    Operator::Equals => "=",
                    Operator::NotEquals => "!=",
                    Operator::LessThan => "<",
                    Operator::GreaterThan => ">",
                    Operator::LessEquals => "<=",
                    Operator::GreaterEquals => ">=",
                    Operator::Contains => "LIKE",
                    Operator::NotContains => "NOT LIKE",
                    Operator::Matches => "REGEXP",
                };

                let param = match op {
                    Operator::Contains | Operator::NotContains => {
                        let pattern = format!("%{}%", value);
                        Self::create_param(&pattern, params)
                    }
                    _ => {
                        let value = match field.as_str() {
                            "File" => {
                                let file_prefix = options.file_prefix.as_deref().unwrap_or("");
                                &format!("{}{}", file_prefix, value)
                            }
                            _ => value,
                        };

                        Self::create_param(value, params)
                    }
                };

                let cloned_options = options.clone();
                let column = cloned_options.columns.get(field).unwrap_or(&field);

                format!("{} {} {}", *column, op_str, param)
            }
            Expression::Logical { left, op, right } => {
                let left_sql = left.to_sql_internal(params, options.clone());
                let right_sql = right.to_sql_internal(params, options.clone());

                match op {
                    LogicalOp::And => format!("{} AND {}", left_sql, right_sql),
                    LogicalOp::Or => format!("{} OR {}", left_sql, right_sql),
                }
            }
            Expression::Group(expr) => format!("({})", expr.to_sql_internal(params, options)),
        }
    }
}

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<char>,
    position: usize,
}

impl Parser {
    pub fn new(input: &str) -> Self {
        Parser {
            tokens: input.chars().collect(),
            position: 0,
        }
    }

    pub fn parse(&mut self) -> Result<Expression, String> {
        self.skip_whitespace();
        self.parse_expression()
    }

    pub fn peek(&self) -> Option<char> {
        self.tokens.get(self.position).copied()
    }

    pub fn next(&mut self) -> Option<char> {
        let c = self.peek();
        self.position += 1;
        c
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            if !c.is_whitespace() {
                break;
            }
            self.next();
        }
    }

    fn parse_expression(&mut self) -> Result<Expression, String> {
        if self.peek() == Some('(') {
            self.next(); // consume '('
            let expr = self.parse_expression()?;
            self.skip_whitespace();

            if self.next() != Some(')') {
                return Err("Expected closing parenthesis".to_string());
            }

            self.skip_whitespace();

            // Check for logical operator after the group
            if let Some(op) = self.parse_logical_op() {
                let right = self.parse_expression()?;
                return Ok(Expression::Logical {
                    left: Box::new(Expression::Group(Box::new(expr))),
                    op,
                    right: Box::new(right),
                });
            }

            return Ok(Expression::Group(Box::new(expr)));
        }

        // Parse field name
        let mut field = String::new();
        while let Some(c) = self.peek() {
            if c.is_alphanumeric() || c == '_' {
                field.push(self.next().unwrap());
            } else {
                break;
            }
        }

        self.skip_whitespace();

        let op = self.parse_operator()?;

        self.skip_whitespace();

        // Parse value
        let value = self.parse_value()?;

        let expr = Expression::Comparaison { field, op, value };

        self.skip_whitespace();

        // Check for logical operators
        if let Some(logical_op) = self.parse_logical_op() {
            let right = self.parse_expression()?;
            Ok(Expression::Logical {
                left: Box::new(expr),
                op: logical_op,
                right: Box::new(right),
            })
        } else {
            Ok(expr)
        }
    }

    fn parse_operator(&mut self) -> Result<Operator, String> {
        let start_pos = self.position;
        let mut op_str = String::new();

        while let Some(c) = self.peek() {
            if c.is_whitespace() || c == '\'' {
                break;
            }
            op_str.push(self.next().unwrap());
        }

        match op_str.as_str() {
            "==" => Ok(Operator::Equals),
            "!=" => Ok(Operator::NotEquals),
            "<" => Ok(Operator::LessThan),
            ">" => Ok(Operator::GreaterThan),
            "<=" => Ok(Operator::LessEquals),
            ">=" => Ok(Operator::GreaterEquals),
            "contains" => Ok(Operator::Contains),
            "!contains" => Ok(Operator::NotContains),
            "=~" => Ok(Operator::Matches),
            _ => {
                self.position = start_pos;
                Err(format!("Unknown operator: {}", op_str))
            }
        }
    }

    fn parse_value(&mut self) -> Result<String, String> {
        self.skip_whitespace();

        if self.next() != Some('\'') {
            return Err("Expected opening quote for value".to_string());
        }

        let mut value = String::new();
        let mut is_escaped = false;

        while let Some(c) = self.next() {
            if is_escaped {
                value.push(c);
                is_escaped = false;
                continue;
            }

            match c {
                '\\' => is_escaped = true,
                '\'' => return Ok(value),
                _ => value.push(c),
            }
        }

        Err("Unterminated value".to_string())
    }

    fn parse_logical_op(&mut self) -> Option<LogicalOp> {
        self.skip_whitespace();

        // Try to parse '&&' or '||' first
        if self.position + 1 < self.tokens.len() {
            match (self.tokens[self.position], self.tokens[self.position + 1]) {
                ('&', '&') => {
                    self.position += 2;
                    self.skip_whitespace();
                    return Some(LogicalOp::And);
                }
                ('|', '|') => {
                    self.position += 2;
                    self.skip_whitespace();
                    return Some(LogicalOp::Or);
                }
                _ => {}
            }
        }

        let mut word = String::new();
        // Try to parse 'AND' or 'OR'
        while let Some(c) = self.peek() {
            if c.is_alphabetic() {
                word.push(self.next().unwrap())
            } else {
                break;
            }
        }

        match word.to_uppercase().as_str() {
            "AND" => {
                self.skip_whitespace();
                Some(LogicalOp::And)
            }
            "OR" => {
                self.skip_whitespace();
                Some(LogicalOp::Or)
            }
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_comparaison() {
        let mut parser = Parser::new("Album == '10 Summers'");
        let result = parser.parse().unwrap();
        assert_eq!(
            result,
            Expression::Comparaison {
                field: "Album".to_string(),
                op: Operator::Equals,
                value: "10 Summers".to_string()
            }
        );
    }

    #[test]
    fn test_grouped_expression() {
        let mut parser = Parser::new("(Album == '10 Summers')");
        let result = parser.parse().unwrap();
        assert_eq!(
            result,
            Expression::Group(Box::new(Expression::Comparaison {
                field: "Album".to_string(),
                op: Operator::Equals,
                value: "10 Summers".to_string()
            }))
        );
    }

    #[test]
    fn test_super_grouped_expression() {
        let mut parser = Parser::new("((Album == '10 Summers'))");
        let result = parser.parse().unwrap();
        assert_eq!(
            result,
            Expression::Group(Box::new(Expression::Group(Box::new(
                Expression::Comparaison {
                    field: "Album".to_string(),
                    op: Operator::Equals,
                    value: "10 Summers".to_string()
                }
            ))))
        );
    }

    #[test]
    fn test_logical_and() {
        let mut parser = Parser::new("Album == '10 Summers' && Artist == 'DJ Mustard'");
        let result = parser.parse().unwrap();
        match result {
            Expression::Logical { op, .. } => assert_eq!(op, LogicalOp::And),
            _ => panic!("Expected logical expression"),
        }

        let mut parser = Parser::new("Album == '10 Summers' AND Artist == 'DJ Mustard'");
        let result = parser.parse().unwrap();
        match result {
            Expression::Logical { op, .. } => assert_eq!(op, LogicalOp::And),
            _ => panic!("Expected logical expression"),
        }
    }

    #[test]
    fn test_logical_or() {
        let mut parser = Parser::new("Album == '10 Summers' || Artist == 'DJ Mustard'");
        let result = parser.parse().unwrap();
        match result {
            Expression::Logical { op, .. } => assert_eq!(op, LogicalOp::Or),
            _ => panic!("Expected logical expression"),
        }

        let mut parser = Parser::new("Album == '10 Summers' OR Artist == 'DJ Mustard'");
        let result = parser.parse().unwrap();
        match result {
            Expression::Logical { op, .. } => assert_eq!(op, LogicalOp::Or),
            _ => panic!("Expected logical expression"),
        }
    }

    #[test]
    fn test_to_sql() {
        let mut parser = Parser::new("Album == '10 Summers' && Artist == 'DJ Mustard'");
        let result = parser.parse().unwrap();
        assert_eq!(
            result.to_sql(SqlOptions::default()),
            (
                "album = $1 AND artist = $2".to_string(),
                vec!["10 Summers".to_string(), "DJ Mustard".to_string()]
            )
        );
    }
    #[test]
    fn test_escaped_str_to_sql() {
        let mut parser = Parser::new(
            "((Artist == 'Best DJ Collection') AND (Album == 'Discobitch \\(C\\'est Beau La Bourgeoisie\\)'))",
        );
        let result = parser.parse().unwrap();
        assert_eq!(
            result.to_sql(SqlOptions::default()),
            (
                "((artist = $1) AND (album = $2))".to_string(),
                vec![
                    "Best DJ Collection".to_string(),
                    "Discobitch (C'est Beau La Bourgeoisie)".to_string()
                ],
            )
        );
    }

    #[test]
    fn test_grouped_to_sql() {
        let mut parser = Parser::new("((Album == '10 Summers' && Artist == 'DJ Mustard') || (Album == 'Discovery' && Artist == 'Daft Punk'))");
        let result = parser.parse().unwrap();
        assert_eq!(
            result.to_sql(SqlOptions::default()),
            (
                "((album = $1 AND artist = $2) OR (album = $3 AND artist = $4))".to_string(),
                vec![
                    "10 Summers".to_string(),
                    "DJ Mustard".to_string(),
                    "Discovery".to_string(),
                    "Daft Punk".to_string()
                ]
            )
        );
    }

    #[test]
    fn test_to_sql_options() {
        let mut parser = Parser::new("Album == '10 Summers' AND Artist == 'DJ Mustard' AND File == '10 Summers/01. Low Low (feat. Nipsey Hussle, TeeCee, and RJ).mp3'");
        let result = parser.parse().unwrap();
        let mut columns = HashMap::new();
        columns.insert("Album".to_string(), "album".to_string());
        columns.insert("Artist".to_string(), "artist".to_string());
        columns.insert("File".to_string(), "path".to_string());

        assert_eq!(
            result.to_sql(SqlOptions {
                columns,
                file_prefix: Some("/home/tsirysndr/Music/".to_string())
            }),
            (
                "album = $1 AND artist = $2 AND path = $3".to_string(),
                vec![
                    "10 Summers".to_string(),
                    "DJ Mustard".to_string(),
                    "/home/tsirysndr/Music/10 Summers/01. Low Low (feat. Nipsey Hussle, TeeCee, and RJ).mp3".to_string()
                ]
            )
        );
    }

    #[test]
    fn test_matches_operator() {
        let mut parser = Parser::new("Artist =~ '.*Daft.*'");
        let result = parser.parse().unwrap();
        assert_eq!(
            result,
            Expression::Comparaison {
                field: "Artist".to_string(),
                op: Operator::Matches,
                value: ".*Daft.*".to_string(),
            }
        )
    }

    #[test]
    fn test_evaluate() {
        let mut record = HashMap::new();
        record.insert("Album".to_string(), "Random Access Memories".to_string());
        record.insert("Artist".to_string(), "Daft Punk".to_string());
        record.insert(
            "File".to_string(),
            "/home/tsirysndr/Music/Random Access Memories/01. Give Life Back to Music.mp3"
                .to_string(),
        );
        record.insert("Title".to_string(), "Give Life Back to Music".to_string());

        let mut parser =
            Parser::new("((Album == 'Random Access Memories') AND (Artist == 'Daft Punk'))");
        let expr = parser.parse().unwrap();
        assert_eq!(expr.evaluate(&record), true);

        let mut parser =
            Parser::new("((Album == 'Random Access Memories') AND (Artist == 'Daft Punky'))");
        let expr = parser.parse().unwrap();
        assert_eq!(expr.evaluate(&record), false);

        let mut parser = Parser::new("((Artist =~ '.*Daft.*'))");
        let expr = parser.parse().unwrap();
        assert_eq!(expr.evaluate(&record), true);

        let mut parser = Parser::new("((any =~ '.*Daft.*'))");
        let expr = parser.parse().unwrap();
        assert_eq!(expr.evaluate(&record), true);
    }
}
