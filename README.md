# tiny-peano

ペアノの公理に基づく自然数と足し算の実装。パーサー付き。

## ビルド・実行

```
stack run
```

## テスト

```
stack test
```

## モジュール構成

- `Nat` - ペアノ自然数の型と演算 (Zero, Succ, add)
- `Lexer` - 字句解析 (skipSpace, lexeme, symbol)
- `Parser` - 算術式のパーサー (Text -> AST)
- `Eval` - 評価器 (AST -> Nat)
- `Lib` - エントリポイント (String -> String)
