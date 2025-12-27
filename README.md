This crate allows the user to represent state in the database using Rust enums. This is achieved
through a proc macro. First the macro looks at your chosen `sql_type`, and then it devises a
corresponding Rust type. The mapping is as follows:

| SQL        | Rust     |
| ---------- | -------- |
| `SmallInt` | `i16`    |
| `Integer`  | `i32`    |
| `Int`      | `i32`    |
| `BigInt`   | `i64`    |
| `VarChar`  | `String` |
| `Text`     | `String` |

The macro then generates three impls: a `FromSql` impl, an `ToSql` impl and a
`TryFrom` impl, which allow conversion between the Sql type an the enum (`FromSql` and `ToSql`),
and from the Rust type into the enum (`TryInto`).

### Usage

```rust
use diesel_enum_case::DbEnum;
use diesel::{deserialize::FromSqlRow, sql_types::{SmallInt, VarChar}};

#[derive(Debug, thiserror::Error)]
#[error("CustomError: {msg}, {status}")]
pub struct CustomError {
    msg: String,
    status: u16,
}

impl CustomError {
    fn not_found(msg: String) -> Self {
        Self {
            msg,
            status: 404,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, AsExpression, FromSqlRow, DbEnum)]
#[diesel(sql_type = SmallInt)]
#[diesel_enum(error_fn = CustomError::not_found)]
#[diesel_enum(error_type = CustomError)]
pub enum Status {
    /// Will be represented as 0.
    Ready,
    /// Will be represented as 1.
    Pending,
}
```

Alternatively you can use strings, which will be cast to lowercase by default. (e.g. `Status::Ready` will be
stored as `"ready"` in the database):

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, AsExpression, FromSqlRow, DbEnum)]
#[diesel(sql_type = VarChar)]
#[diesel_enum(error_fn = CustomError::not_found)]
#[diesel_enum(error_type = CustomError)]
pub enum Status {
    /// Will be represented as `"ready"`.
    Ready,
    /// Will be represented as `"pending"`.
    Pending,
}
```

### Case Conversion

For string types, you can specify a case convention using the `case` attribute. Supported options are:
- `lowercase` (default) - e.g., `ReadyToGo` → `"readytogo"`
- `UPPERCASE` - e.g., `ReadyToGo` → `"READYTOGO"`
- `camelCase` - e.g., `ReadyToGo` → `"readyToGo"`
- `PascalCase` - e.g., `ReadyToGo` → `"ReadyToGo"`
- `snake_case` - e.g., `ReadyToGo` → `"ready_to_go"`
- `SCREAMING_SNAKE_CASE` - e.g., `ReadyToGo` → `"READY_TO_GO"`
- `kebab-case` - e.g., `ReadyToGo` → `"ready-to-go"`

Example using `snake_case`:

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, AsExpression, FromSqlRow, DbEnum)]
#[diesel(sql_type = VarChar)]
#[diesel_enum(error_fn = CustomError::not_found)]
#[diesel_enum(error_type = CustomError)]
#[diesel_enum(case = "snake_case")]
pub enum Status {
    /// Will be represented as `"ready_to_go"`.
    ReadyToGo,
    /// Will be represented as `"pending_review"`.
    PendingReview,
}
```

The `case` attribute works together with the `val` attribute - if a variant has an explicit `val` attribute, it will override the case conversion for that specific variant.

Another option is to manually override the values set for each or some of the variants. This is done
using the `val` attribute:

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, AsExpression, FromSqlRow, DbEnum)]
#[diesel(sql_type = VarChar)]
#[diesel_enum(error_fn = CustomError::not_found)]
#[diesel_enum(error_type = CustomError)]
pub enum Status {
    /// Will be represented as `"reddy"`.
    #[val = "reddy"]
    Ready,
    /// Will be represented as `"pending"`.
    Pending,
}
```
