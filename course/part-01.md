# Part 01: การติดตั้ง Haskell และ GHC
## ขั้นตอนที่ 1-20: เริ่มต้นการเดินทางสู่ Haskell

---

## บทนำ

Haskell เป็นภาษาโปรแกรมมิ่งเชิงฟังก์ชัน (Functional Programming Language) ที่มีระบบ Type ที่แข็งแกร่งมาก มันถูกออกแบบมาเพื่อให้เขียนโปรแกรมที่ถูกต้องและมีประสิทธิภาพสูง ก่อนที่เราจะเริ่มเรียนรู้ภาษา เราต้องติดตั้งเครื่องมือที่จำเป็นก่อน

### ทำไมต้องเรียน Haskell?

Haskell มีคุณสมบัติพิเศษที่ทำให้มันโดดเด่นจากภาษาอื่น:

1. **Pure Functions** - ฟังก์ชันที่ให้ผลลัพธ์เดิมเสมอสำหรับ input เดิม
2. **Strong Static Typing** - ตรวจสอบ Type ตั้งแต่ compile time ทำให้ลด bugs
3. **Type Inference** - คอมไพเลอร์สามารถ infer types ได้เอง ไม่ต้องระบุทุกที่
4. **Lazy Evaluation** - ประมวลผลเฉพาะส่วนที่จำเป็นจริงๆ
5. **Concurrency** - จัดการ concurrency ได้อย่างปลอดภัยด้วย STM
6. **Expressive** - เขียนโค้ดน้อยแต่ได้ผลลัพธ์มาก

---

## ขั้นตอนที่ 1: ติดตั้ง GHCup

GHCup เป็นเครื่องมือสำหรับจัดการ Haskell toolchain รวมถึง GHC (Glasgow Haskell Compiler), Cabal, Stack และ HLS (Haskell Language Server)

### บน Linux/macOS

เปิด Terminal และรันคำสั่งนี้:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

ระหว่างการติดตั้ง จะมีคำถามหลายข้อ ให้ตอบดังนี้:
- เพิ่ม GHCup ใน PATH: **Y**
- ติดตั้ง Haskell Language Server (HLS): **Y** (แนะนำสำหรับการพัฒนา)
- ติดตั้ง Stack: **Y**

หลังจากติดตั้งเสร็จ ให้ reload terminal หรือรัน:

```bash
source ~/.ghcup/env
```

### บน Windows

ดาวน์โหลด GHCup installer จาก https://www.haskell.org/ghcup/#

หรือใช้ PowerShell (ต้องรัน as Administrator):

```powershell
Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { & ([scriptblock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -Interactive -DisableCurl } catch { Write-Error $_ }
```

---

## ขั้นตอนที่ 2: ตรวจสอบการติดตั้ง

หลังจากติดตั้งเสร็จ ให้ตรวจสอบว่าทุกอย่างทำงานได้:

```bash
# ตรวจสอบ GHC version
ghc --version
# ควรแสดง: The Glorious Glasgow Haskell Compilation System, version 9.x.x

# ตรวจสอบ GHCi (Haskell REPL)
ghci --version
# ควรแสดง version เดียวกับ GHC

# ตรวจสอบ Cabal
cabal --version
# ควรแสดง: cabal-install version x.x.x.x

# ตรวจสอบ Stack (ถ้าติดตั้ง)
stack --version
# ควรแสดง: Version x.x.x

# ตรวจสอบ HLS (ถ้าติดตั้ง)
haskell-language-server-wrapper --version
```

---

## ขั้นตอนที่ 3: ทำความรู้จักกับ GHCi (Haskell REPL)

GHCi คือ Interactive interpreter สำหรับ Haskell เราสามารถทดสอบโค้ดได้โดยตรงโดยไม่ต้องสร้างไฟล์

```bash
# เริ่ม GHCi
ghci
```

คุณจะเห็น prompt แบบนี้:
```
GHCi, version 9.4.7: https://www.haskell.org/ghc/  :? for help
ghci>
```

### คำสั่งพื้นฐานใน GHCi

```haskell
-- คำนวณพื้นฐาน
ghci> 2 + 3
5

ghci> 10 * 5
50

ghci> 100 / 4
25.0

ghci> 7 `div` 2    -- หารจำนวนเต็ม
3

ghci> 7 `mod` 2    -- หารเอาเศษ
1

-- ทำงานกับ String
ghci> "Hello, World!"
"Hello, World!"

ghci> "Hello" ++ " " ++ "World"
"Hello World"

-- ค่า Boolean
ghci> True && False
False

ghci> True || False
True

ghci> not True
False

-- เปรียบเทียบ
ghci> 5 > 3
True

ghci> 5 == 5
True

ghci> 5 /= 3    -- ไม่เท่ากับ
True
```

### คำสั่ง GHCi ที่สำคัญ

```haskell
-- แสดงความช่วยเหลือ
ghci> :?

-- ตรวจสอบ Type ของ expression
ghci> :type "Hello"
"Hello" :: String

ghci> :t 42
42 :: Num a => a

ghci> :t True
True :: Bool

-- โหลดไฟล์
ghci> :load MyFile.hs
-- หรือ
ghci> :l MyFile.hs

-- Reload ไฟล์ที่โหลดอยู่
ghci> :reload
-- หรือ
ghci> :r

-- ออกจาก GHCi
ghci> :quit
-- หรือ
ghci> :q
-- หรือกด Ctrl+D
```

---

## ขั้นตอนที่ 4: สร้าง Haskell Project แรกด้วย Cabal

### สร้าง Project ใหม่

```bash
# สร้าง directory ใหม่
mkdir hello-haskell
cd hello-haskell

# สร้าง project structure
cabal init --interactive
```

Cabal จะถามคำถามหลายข้อ:

```
What does the package build:
   1) Executable
   2) Library
   3) Library and Executable
Your choice? [default: Executable] 1

What is the main module of the executable:
 * 1) Main (recommended)
   2) Other
Your choice? [default: Main (recommended)] 1

Package name? [default: hello-haskell]
Package version? [default: 0.1.0.0]
Please choose a license:
 * 1) NONE
   2) BSD-2-Clause
   3) BSD-3-Clause
   ...
Your choice? [default: NONE] 1
Author name? Your Name
Maintainer email? your@email.com
Project homepage URL?
Project synopsis? My first Haskell project
...
```

### โครงสร้างไฟล์

หลังจาก init เสร็จ คุณจะได้:

```
hello-haskell/
├── hello-haskell.cabal    -- Project configuration
├── app/
│   └── Main.hs            -- Entry point
└── CHANGELOG.md
```

### ไฟล์ hello-haskell.cabal

```cabal
cabal-version:      3.0
name:               hello-haskell
version:            0.1.0.0
synopsis:           My first Haskell project
license:            NONE
author:             Your Name
maintainer:         your@email.com
build-type:         Simple

executable hello-haskell
    main-is:          Main.hs
    build-depends:    base ^>=4.17.0.0
    hs-source-dirs:   app
    default-language: Haskell2010
```

### ไฟล์ app/Main.hs

สร้างไฟล์ Main.hs:

```haskell
module Main where

main :: IO ()
main = putStrLn "Hello, Haskell World!"
```

### รัน Project

```bash
# Build และรัน
cabal run

# หรือ build ก่อน แล้วรันทีหลัง
cabal build
cabal run hello-haskell
```

Output:
```
Hello, Haskell World!
```

---

## ขั้นตอนที่ 5: สร้าง Project ด้วย Stack

Stack เป็นอีกทางเลือกหนึ่งในการจัดการ Haskell project

```bash
# สร้าง project ใหม่
stack new hello-stack simple

# เข้าไปใน directory
cd hello-stack

# โครงสร้าง
# hello-stack/
# ├── stack.yaml          -- Stack configuration
# ├── hello-stack.cabal   -- Package configuration
# ├── src/
# │   └── Main.hs         -- Main source file
# └── test/
#     └── Spec.hs         -- Test file
```

### Build และ Run ด้วย Stack

```bash
# Build project
stack build

# รัน project
stack exec hello-stack-exe

# หรือ build และ run พร้อมกัน
stack run
```

---

## ขั้นตอนที่ 6: ตั้งค่า Editor/IDE

### VS Code (แนะนำ)

1. ติดตั้ง [VS Code](https://code.visualstudio.com/)
2. ติดตั้ง Extension **Haskell** (publisher: Haskell)
3. Extension นี้จะใช้ HLS (Haskell Language Server) โดยอัตโนมัติ

คุณสมบัติที่ได้:
- Auto-completion
- Type checking แบบ real-time
- Go to definition
- Show documentation
- Code formatting

### Settings สำหรับ VS Code

เพิ่มใน `settings.json`:

```json
{
  "haskell.formattingProvider": "fourmolu",
  "haskell.serverEnvironment": {
    "PATH": "${HOME}/.ghcup/bin:${PATH}"
  },
  "editor.formatOnSave": true,
  "[haskell]": {
    "editor.defaultFormatter": "haskell.haskell"
  }
}
```

### Neovim (สำหรับผู้ที่ชอบ Terminal)

ใช้ nvim-lspconfig กับ haskell-tools.nvim:

```lua
-- lazy.nvim configuration
{
  'mrcjkb/haskell-tools.nvim',
  version = '^3',
  ft = { 'haskell', 'lhaskell', 'cabal', 'cabalproject' },
}
```

### IntelliJ IDEA

ติดตั้ง IntelliJ-Haskell plugin จาก Plugin Marketplace

---

## ขั้นตอนที่ 7: ติดตั้ง Tools เพิ่มเติม

### Formatter: Fourmolu

```bash
# ติดตั้งผ่าน Cabal
cabal install fourmolu

# หรือผ่าน GHCup
ghcup install tool fourmolu

# ใช้งาน
fourmolu --mode inplace MyFile.hs
```

### Linter: HLint

```bash
# ติดตั้ง
cabal install hlint

# ใช้งาน
hlint MyFile.hs

# ตรวจสอบทั้ง project
hlint .
```

### ตัวอย่าง HLint suggestions

```haskell
-- โค้ดเดิม (อาจ redundant)
map (\x -> x + 1) [1,2,3]

-- HLint จะแนะนำให้ใช้
map (+1) [1,2,3]
```

---

## ขั้นตอนที่ 8: เข้าใจ GHC Extensions

Haskell มี extensions หลายตัวที่เพิ่มความสามารถ เราจะใช้บางส่วนในหลักสูตรนี้

### วิธีเปิด Extension

**ใน Source File:**
```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where
```

**ใน .cabal File:**
```cabal
executable myapp
    default-extensions:
        OverloadedStrings
        ScopedTypeVariables
    ...
```

### Extensions ที่ใช้บ่อย

```haskell
-- OverloadedStrings: ทำให้ string literals ทำงานกับ Text, ByteString ได้
{-# LANGUAGE OverloadedStrings #-}

-- RecordWildCards: Unpack record fields โดยอัตโนมัติ
{-# LANGUAGE RecordWildCards #-}

-- TupleSections: ใช้ shorthand สำหรับ tuple sections
{-# LANGUAGE TupleSections #-}

-- LambdaCase: ใช้ case ใน lambda ได้สั้นลง
{-# LANGUAGE LambdaCase #-}

-- ScopedTypeVariables: ให้ type variables มี scope ที่ชัดเจนขึ้น
{-# LANGUAGE ScopedTypeVariables #-}

-- TypeApplications: ระบุ type arguments โดยตรง
{-# LANGUAGE TypeApplications #-}

-- DerivingStrategies: ควบคุม deriving strategy
{-# LANGUAGE DerivingStrategies #-}
```

---

## ขั้นตอนที่ 9: โปรแกรมแรกที่ซับซ้อนขึ้น

```haskell
-- app/Main.hs
module Main where

import Data.List (sort, nub)
import System.IO

-- | ฟังก์ชันสำหรับทักทาย
greet :: String -> String
greet name = "สวัสดี, " ++ name ++ "!"

-- | คำนวณ Fibonacci
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- | หาค่า factorial
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- | ตรวจสอบว่าเป็นจำนวนเฉพาะ
isPrime :: Int -> Bool
isPrime n
  | n < 2     = False
  | n == 2    = True
  | even n    = False
  | otherwise = all (\i -> n `mod` i /= 0) [3, 5..isqrt n]
  where
    isqrt = floor . sqrt . fromIntegral

-- | หาจำนวนเฉพาะในช่วงที่กำหนด
primesInRange :: Int -> Int -> [Int]
primesInRange start end = filter isPrime [start..end]

main :: IO ()
main = do
  -- ทักทาย
  putStrLn (greet "Haskell Learner")

  -- Fibonacci
  putStrLn "\nลำดับ Fibonacci 10 ตัวแรก:"
  print (map fibonacci [0..9])

  -- Factorial
  putStrLn "\nFactorial ของ 1 ถึง 10:"
  mapM_ (\n -> putStrLn $ show n ++ "! = " ++ show (factorial n)) [1..10]

  -- จำนวนเฉพาะ
  putStrLn "\nจำนวนเฉพาะระหว่าง 1 ถึง 50:"
  print (primesInRange 1 50)
```

รัน:
```bash
cabal run
```

Output:
```
สวัสดี, Haskell Learner!

ลำดับ Fibonacci 10 ตัวแรก:
[0,1,1,2,3,5,8,13,21,34]

Factorial ของ 1 ถึง 10:
1! = 1
2! = 2
3! = 6
4! = 24
5! = 120
6! = 720
7! = 5040
8! = 40320
9! = 362880
10! = 3628800

จำนวนเฉพาะระหว่าง 1 ถึง 50:
[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47]
```

---

## ขั้นตอนที่ 10: การจัดการ Dependencies

### เพิ่ม Dependencies ใน .cabal

```cabal
executable hello-haskell
    main-is:          Main.hs
    build-depends:
        base            ^>=4.17.0.0,
        text            ^>=2.0,
        bytestring      ^>=0.11,
        containers      ^>=0.6,
        aeson           ^>=2.1,
        time            ^>=1.12
    hs-source-dirs:   app
    default-language: Haskell2010
```

### อัพเดท Dependencies

```bash
# อัพเดท package index
cabal update

# Build (จะดาวน์โหลด dependencies โดยอัตโนมัติ)
cabal build
```

### ตรวจสอบ Dependencies ที่มี

```bash
# แสดง packages ที่ติดตั้งแล้ว
cabal list --installed

# ค้นหา package
cabal list text

# แสดงข้อมูลของ package
cabal info aeson
```

---

## ขั้นตอนที่ 11: การใช้ Hackage

[Hackage](https://hackage.haskell.org/) คือ central package repository ของ Haskell เหมือน npm ของ JavaScript หรือ PyPI ของ Python

### การค้นหา Package

```bash
# ค้นหาออนไลน์ที่ hackage.haskell.org
# หรือใช้ command line
cabal list json

# หรือใช้ Hoogle สำหรับค้นหาตาม type signature
# hoogle.haskell.org
```

### Package ที่ใช้บ่อยในหลักสูตร

| Package | ความสามารถ |
|---------|-----------|
| `text` | Unicode text processing |
| `bytestring` | Efficient binary/text data |
| `aeson` | JSON encoding/decoding |
| `containers` | Maps, Sets, Sequences |
| `vector` | Efficient arrays |
| `mtl` | Monad transformers |
| `transformers` | Standard monad transformers |
| `servant` | Type-safe REST APIs |
| `warp` | Web server |
| `persistent` | Database ORM |
| `yesod` | Web framework |
| `hspec` | Testing framework |
| `QuickCheck` | Property-based testing |

---

## ขั้นตอนที่ 12: Stack vs Cabal - เลือกอะไรดี?

### Cabal
**ข้อดี:**
- เป็น official build tool ของ Haskell
- ทันสมัยกว่า (version ล่าสุดมี features มาก)
- ไม่ต้องจัดการ snapshots

**ข้อเสีย:**
- "Cabal hell" ในอดีต (ตอนนี้ดีขึ้นมากด้วย nix-style builds)

### Stack
**ข้อดี:**
- Reproducible builds ด้วย LTS snapshots
- จัดการ GHC versions โดยอัตโนมัติ
- ง่ายสำหรับมือใหม่

**ข้อเสีย:**
- ใช้ space มากกว่าเพราะเก็บหลาย GHC versions

### คำแนะนำ

สำหรับหลักสูตรนี้ เราจะใช้ **Cabal** เป็นหลัก เพราะ:
1. เป็น official tool
2. Version ล่าสุดทำงานได้ดีมาก
3. ตรงกับ documentation ส่วนใหญ่

---

## ขั้นตอนที่ 13: โครงสร้าง Project ที่ดี

```
my-project/
├── my-project.cabal          -- Package configuration
├── cabal.project             -- Project configuration (optional)
├── src/                      -- Library source
│   ├── MyLib/
│   │   ├── Core.hs
│   │   ├── Types.hs
│   │   └── Utils.hs
│   └── MyLib.hs              -- Re-exports
├── app/                      -- Executable source
│   └── Main.hs
├── test/                     -- Test source
│   ├── Spec.hs
│   └── MyLib/
│       ├── CoreSpec.hs
│       └── UtilsSpec.hs
├── bench/                    -- Benchmarks (optional)
│   └── Main.hs
└── README.md
```

### ตัวอย่าง .cabal สำหรับ Project ใหญ่

```cabal
cabal-version:      3.0
name:               my-project
version:            0.1.0.0
synopsis:           A comprehensive Haskell project
description:        Longer description here
license:            MIT
license-file:       LICENSE
author:             Your Name
maintainer:         your@email.com
category:           Web
build-type:         Simple
extra-source-files: README.md

common warnings
    ghc-options: -Wall -Wcompat -Widentities -Wincomplete-record-updates

library
    import:           warnings
    exposed-modules:
        MyLib
        MyLib.Core
        MyLib.Types
        MyLib.Utils
    other-modules:
        MyLib.Internal
    build-depends:
        base        ^>=4.17
        , text      ^>=2.0
        , containers ^>=0.6
    hs-source-dirs:   src
    default-language: Haskell2010
    default-extensions:
        OverloadedStrings
        ScopedTypeVariables

executable my-project
    import:           warnings
    main-is:          Main.hs
    build-depends:
        base       ^>=4.17
        , my-project
    hs-source-dirs:   app
    default-language: Haskell2010

test-suite my-project-test
    import:           warnings
    default-language: Haskell2010
    type:             exitcode-stdio-1.0
    hs-source-dirs:   test
    main-is:          Spec.hs
    build-depends:
        base        ^>=4.17
        , my-project
        , hspec     ^>=2.10
        , QuickCheck ^>=2.14
```

---

## ขั้นตอนที่ 14: การเขียน Haskell File แรก

สร้างไฟล์ `src/Hello.hs`:

```haskell
-- | Module สำหรับ Hello World functions
module Hello where

-- | นี่คือ type signature ของฟังก์ชัน
-- String -> String หมายถึงรับ String และคืน String
greetInThai :: String -> String
greetInThai name = "สวัสดี, " ++ name ++ "! ยินดีต้อนรับสู่ Haskell!"

greetInEnglish :: String -> String
greetInEnglish name = "Hello, " ++ name ++ "! Welcome to Haskell!"

-- | ฟังก์ชันที่รับ list ของชื่อ
greetAll :: [String] -> [String]
greetAll names = map greetInEnglish names

-- | ตัวอย่างการใช้ where clause
greetWithTitle :: String -> String -> String
greetWithTitle title name = greeting
  where
    greeting = "Hello, " ++ fullName ++ "!"
    fullName = title ++ " " ++ name
```

โหลดใน GHCi:

```bash
ghci
ghci> :load src/Hello.hs
[1 of 1] Compiling Hello            ( src/Hello.hs, interpreted )
Ok, one module loaded.
ghci> greetInThai "สมชาย"
"สวัสดี, สมชาย! ยินดีต้อนรับสู่ Haskell!"
ghci> greetAll ["Alice", "Bob", "Charlie"]
["Hello, Alice! Welcome to Haskell!","Hello, Bob! Welcome to Haskell!","Hello, Charlie! Welcome to Haskell!"]
ghci> greetWithTitle "Dr." "Smith"
"Hello, Dr. Smith!"
```

---

## ขั้นตอนที่ 15: ความเข้าใจเรื่อง Purity

สิ่งที่ทำให้ Haskell พิเศษคือ **Purity** - ฟังก์ชันใน Haskell จะไม่มี side effects (ยกเว้นที่ระบุชัดเจนผ่าน IO Monad)

### Pure Function

```haskell
-- pure: ให้ผลลัพธ์เดิมเสมอสำหรับ input เดิม
add :: Int -> Int -> Int
add x y = x + y

double :: Int -> Int
double x = x * 2

-- ตัวอย่างการใช้งาน
result1 = add 3 4      -- เสมอได้ 7
result2 = double 5     -- เสมอได้ 10
```

### Impure Function (IO)

```haskell
-- impure: มี side effects (อ่าน/เขียน, I/O, etc.)
main :: IO ()
main = do
  putStrLn "What is your name?"    -- เขียนออก console (side effect)
  name <- getLine                   -- อ่านจาก console (side effect)
  putStrLn ("Hello, " ++ name)
```

### ทำไม Purity ถึงสำคัญ?

```haskell
-- Referential Transparency: เราสามารถแทนที่ expression ด้วยค่าของมัน
result = add (double 3) (add 1 2)
-- เทียบเท่ากับ
result = add 6 3    -- double 3 = 6
-- เทียบเท่ากับ
result = 9          -- add 6 3 = 9

-- ใน Haskell เราสามารถ reason about code ได้ง่ายกว่า
-- เพราะ function จะไม่เปลี่ยนแปลง state ภายนอก
```

---

## ขั้นตอนที่ 16: ทำความเข้าใจ Lazy Evaluation

Haskell ใช้ **Lazy Evaluation** หรือ Call-by-need หมายถึงจะคำนวณค่าต่อเมื่อจำเป็นจริงๆ

```haskell
-- infinite list! ใน Haskell ทำได้เพราะ lazy evaluation
naturals :: [Int]
naturals = [1..]

-- เราสามารถเอาแค่ 10 ตัวแรก
first10 :: [Int]
first10 = take 10 naturals
-- [1,2,3,4,5,6,7,8,9,10]

-- Fibonacci sequence แบบ infinite
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- เอาแค่ที่ต้องการ
first20Fibs :: [Integer]
first20Fibs = take 20 fibs
-- [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181]

-- หา fibonacci ที่มากกว่า 1000
fibsOver1000 :: [Integer]
fibsOver1000 = filter (> 1000) (take 30 fibs)
```

```bash
ghci> take 10 [1..]
[1,2,3,4,5,6,7,8,9,10]

ghci> let fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
ghci> take 15 fibs
[0,1,1,2,3,5,8,13,21,34,55,89,144,233,377]
```

---

## ขั้นตอนที่ 17: GHCi Tips และ Tricks

### Multi-line Input

```haskell
ghci> :{
ghci|  let double x = x * 2
ghci|      triple x = x * 3
ghci| :}
ghci> double 5
10
ghci> triple 5
15
```

### ใช้ let ใน GHCi

```haskell
ghci> let x = 10
ghci> let y = 20
ghci> x + y
30
ghci> let square n = n * n
ghci> square 7
49
```

### ตั้งค่า GHCi

สร้างไฟล์ `~/.ghci`:

```haskell
:set prompt "λ> "
:set +t         -- แสดง type หลังจาก evaluate
:set +s         -- แสดง stats (time, memory)
```

---

## ขั้นตอนที่ 18: Error Messages ใน Haskell

Haskell มี error messages ที่ค่อนข้างยาวและซับซ้อน แต่ถ้าอ่านเข้าใจจะช่วยได้มาก

### Type Error ทั่วไป

```haskell
-- พยายามบวก String กับ Int
ghci> "hello" + 5

-- Error message:
-- <interactive>:1:1: error:
--     • No instance for (Num String) arising from a use of '+'
--     • In the expression: "hello" + 5
--       In an equation for 'it': it = "hello" + 5
```

### การอ่าน Error Message

```
No instance for (Num String)
```
หมายความว่า: ไม่มี implementation ของ Num typeclass สำหรับ String (ไม่สามารถบวก String ได้)

```haskell
-- อีกตัวอย่าง
ghci> [1, 2, 3] ++ 4

-- Error:
-- • Couldn't match type 'Int' with '[Int]'
-- Expected: [[Int]]
--   Actual: [Int]
```

หมายความว่า: ++ ต้องการ list ทั้งสองด้าน แต่ด้านขวาเป็น Int ไม่ใช่ [Int]

---

## ขั้นตอนที่ 19: Debugging ใน Haskell

### ใช้ Debug.Trace

```haskell
import Debug.Trace

-- trace พิมพ์ message แล้วคืนค่าเดิม
myFunc :: Int -> Int
myFunc n = trace ("myFunc called with " ++ show n) (n * 2)

main :: IO ()
main = do
  let result = myFunc 5
  print result

-- Output:
-- myFunc called with 5
-- 10
```

### ใช้ error สำหรับ debugging

```haskell
-- ใช้ error เพื่อหยุดโปรแกรมพร้อม message
safeDiv :: Int -> Int -> Int
safeDiv _ 0 = error "Division by zero!"
safeDiv x y = x `div` y
```

### ใช้ undefined

```haskell
-- undefined สำหรับ placeholder ที่ยังไม่ implement
myFunction :: Int -> String
myFunction n = undefined  -- ยังไม่ implement
```

---

## ขั้นตอนที่ 20: สรุปและทบทวน

### สิ่งที่เรียนรู้ใน Part นี้

1. ✅ ติดตั้ง GHCup, GHC, Cabal, Stack
2. ✅ ใช้งาน GHCi (interactive REPL)
3. ✅ สร้าง Haskell project ด้วย Cabal และ Stack
4. ✅ ตั้งค่า Editor ด้วย HLS
5. ✅ เข้าใจ Purity และ Lazy Evaluation เบื้องต้น
6. ✅ อ่าน Error Messages

### แบบฝึกหัด

1. ติดตั้ง GHCup และตรวจสอบว่าทุกอย่างทำงาน
2. เปิด GHCi และลองคำนวณ: `sum [1..100]`, `product [1..10]`
3. สร้าง Cabal project ใหม่ชื่อ "learning-haskell"
4. เขียนฟังก์ชัน `circleArea :: Double -> Double` ที่คำนวณพื้นที่วงกลม
5. เขียน infinite list ของจำนวนคี่: `[1,3,5,7...]` โดยใช้ `filter`

### คำตอบแบบฝึกหัด

```haskell
-- ข้อ 2
ghci> sum [1..100]
5050

ghci> product [1..10]
3628800

-- ข้อ 4
circleArea :: Double -> Double
circleArea r = pi * r * r

-- ข้อ 5
odds :: [Int]
odds = filter odd [1..]

-- หรือ
odds' :: [Int]
odds' = [1, 3..]
```

---

## สิ่งที่จะเรียนใน Part ถัดไป

ใน Part 02 เราจะเรียนเรื่อง **Types พื้นฐานและ Expressions** ใน Haskell ซึ่งรวมถึง:
- Basic Types: Int, Integer, Double, Bool, Char, String
- Type Signatures
- Type Inference
- Let และ Where Expressions
- If-Then-Else

---

*[ต่อ → Part 02: Types พื้นฐานและ Expressions](part-02.md)*
