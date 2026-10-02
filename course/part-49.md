# Part 49: Nix & Advanced Build Systems
## ขั้นตอนที่ 961-980

---

## ขั้นตอนที่ 961: Nix Fundamentals

```nix
# Nix language basics
# Everything in Nix is an expression

# Primitive types
let
  anInteger = 42;
  aFloat    = 3.14;
  aString   = "Hello, World!";
  aPath     = /usr/bin/hello;
  aBool     = true;
  aNull     = null;
in {
  inherit anInteger aFloat aString aBool;
}

# Lists
let
  myList = [ 1 2 3 "four" true ];
  len    = builtins.length myList;  # 5
in myList

# Attribute sets (records)
let
  person = {
    name = "Alice";
    age  = 30;
    address = {
      city    = "Bangkok";
      country = "Thailand";
    };
  };
in person.address.city  # "Bangkok"

# Functions
let
  add  = x: y: x + y;
  greet = { name, greeting ? "Hello" }: "${greeting}, ${name}!";
in {
  sum    = add 3 4;     # 7
  hello  = greet { name = "Bob"; };
  salut  = greet { name = "Alice"; greeting = "Bonjour"; };
}

# Let expressions
let
  x = 10;
  y = x * 2;
  z = x + y;
in z  # 30

# With expression
with builtins; {
  len = length [ 1 2 3 ];  # 3 (builtins.length)
}
```

---

## ขั้นตอนที่ 962: Nix Derivations

```nix
# Nix derivations: how packages are built

# Basic derivation
{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation {
  name    = "hello-1.0";
  version = "1.0.0";
  
  # Source
  src = pkgs.fetchurl {
    url    = "https://example.com/hello-1.0.tar.gz";
    sha256 = "0abc123...";
  };
  
  # Build dependencies
  buildInputs = with pkgs; [
    gcc
    make
    openssl
    zlib
  ];
  
  # Build phases
  configurePhase = ''
    ./configure --prefix=$out
  '';
  
  buildPhase = ''
    make -j$NIX_BUILD_CORES
  '';
  
  installPhase = ''
    make install
  '';
  
  # Metadata
  meta = with pkgs.lib; {
    description = "A simple hello world program";
    license     = licenses.mit;
    platforms   = platforms.linux ++ platforms.darwin;
  };
}

# Haskell package derivation
{ pkgs ? import <nixpkgs> {} }:
let
  haskellPkgs = pkgs.haskellPackages;
in
  haskellPkgs.callCabal2nix "my-package" ./. {
    # Haskell dependencies are automatically resolved
  }
```

---

## ขั้นตอนที่ 963: Flakes

```nix
# flake.nix - modern Nix with flakes

{
  description = "Haskell Servant Application";

  inputs = {
    nixpkgs.url     = "github:NixOS/nixpkgs/nixos-23.11";
    flake-utils.url = "github:numtide/flake-utils";
    haskell-updates.url = "github:NixOS/nixpkgs/haskell-updates";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlays.default ];
        };
        
        hsPkgs = pkgs.haskellPackages;
      in
      {
        # Development shell
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            (hsPkgs.ghcWithPackages (p: with p; [
              servant
              servant-server
              warp
              aeson
              postgresql-simple
              hspec
            ]))
            hsPkgs.cabal-install
            hsPkgs.haskell-language-server
            hsPkgs.hlint
            hsPkgs.ormolu
            postgresql
          ];
          
          shellHook = ''
            echo "Haskell dev environment ready"
            export DATABASE_URL="postgresql://localhost/mydb"
          '';
        };
        
        # Build package
        packages.default = hsPkgs.callCabal2nix "my-servant-app" ./. {};
        
        # Docker image
        packages.docker = pkgs.dockerTools.buildLayeredImage {
          name = "my-servant-app";
          tag  = "latest";
          contents = [ self.packages.${system}.default ];
          config.Cmd = [ "/bin/my-servant-app" ];
        };
      }
    )
    // {
      overlays.default = final: prev: {
        haskellPackages = prev.haskellPackages.override {
          overrides = hfinal: hprev: {
            # Pin specific versions or apply patches
          };
        };
      };
    };
}
```

---

## ขั้นตอนที่ 964: NixOS Configuration

```nix
# NixOS system configuration

{ config, pkgs, lib, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./modules/postgresql.nix
    ./modules/nginx.nix
  ];

  # System packages
  environment.systemPackages = with pkgs; [
    git
    vim
    htop
    curl
    jq
  ];

  # Services
  services = {
    postgresql = {
      enable  = true;
      package = pkgs.postgresql_15;
      ensureDatabases = [ "myapp" ];
      ensureUsers = [{
        name              = "myapp";
        ensurePermissions = { "DATABASE myapp" = "ALL PRIVILEGES"; };
      }];
    };

    nginx = {
      enable = true;
      virtualHosts."api.example.com" = {
        enableACME = true;
        forceSSL   = true;
        locations."/" = {
          proxyPass = "http://127.0.0.1:8080";
          extraConfig = ''
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
          '';
        };
      };
    };
  };

  # Systemd service for Haskell app
  systemd.services.myapp = {
    description  = "My Haskell Application";
    wantedBy     = [ "multi-user.target" ];
    after        = [ "network.target" "postgresql.service" ];
    serviceConfig = {
      ExecStart    = "${pkgs.myapp}/bin/myapp";
      User         = "myapp";
      Restart      = "on-failure";
      RestartSec   = "5s";
      Environment  = [ "PORT=8080" "DATABASE_URL=postgresql://myapp@localhost/myapp" ];
    };
  };

  # Firewall
  networking.firewall = {
    enable          = true;
    allowedTCPPorts = [ 22 80 443 ];
  };
}
```

---

## ขั้นตอนที่ 965: Haskell-specific Nix Tooling

```nix
# haskell.nix - IOG's Haskell build infrastructure

# shell.nix using haskell.nix
let
  haskellNix = import (builtins.fetchTarball {
    url    = "https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz";
    sha256 = "abc123...";
  }) {};
  
  nixpkgs = haskellNix.sources.nixpkgs;
  
  pkgs = import nixpkgs {
    overlays = [ haskellNix.overlay ];
    config   = haskellNix.config;
  };
  
  project = pkgs.haskell-nix.project {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "my-project";
      src  = ./.;
    };
    
    compiler-nix-name = "ghc965";
    
    shell.tools = {
      cabal                      = "latest";
      haskell-language-server    = "latest";
      hlint                      = "latest";
    };
    
    modules = [{
      packages.my-package.components.tests.my-test = {
        testWrapper = [ "bash" "-c" ];
      };
    }];
  };
in

project.shellFor {
  exactDeps = true;
  withHoogle = true;
}

# cabal2nix usage
# Generate nix expression from cabal file:
# cabal2nix . > default.nix

# default.nix generated by cabal2nix
{ mkDerivation, base, servant, servant-server, warp, aeson
, postgresql-simple, hspec, stdenv }:
mkDerivation {
  pname   = "my-servant-app";
  version = "0.1.0.0";
  src     = ./.;
  isLibrary    = true;
  isExecutable = true;
  libraryHaskellDepends    = [ base servant servant-server aeson postgresql-simple ];
  executableHaskellDepends = [ base warp ];
  testHaskellDepends       = [ base hspec ];
  homepage = "https://github.com/example/my-servant-app";
  license  = stdenv.lib.licenses.mit;
}
```

---

## ขั้นตอนที่ 966: Cabal Build System Advanced

```cabal
-- Advanced cabal configuration

cabal-version:      3.0
name:               my-project
version:            0.1.0.0
synopsis:           Advanced Haskell Project
description:
    A comprehensive Haskell application with Servant and Yesod.
license:            MIT
license-file:       LICENSE
author:             Developer
maintainer:         dev@example.com
build-type:         Simple

-- Source repository
source-repository head
  type:     git
  location: https://github.com/example/my-project.git

-- Common settings
common common-settings
  default-language:    Haskell2010
  default-extensions:
    OverloadedStrings
    DeriveGeneric
    RecordWildCards
    LambdaCase
    TupleSections
    TypeApplications
    ScopedTypeVariables
  ghc-options:
    -Wall
    -Wcompat
    -Widentities
    -Wincomplete-record-updates
    -Wincomplete-uni-patterns
    -Wmissing-export-lists
    -Wmissing-home-modules
    -Wpartial-fields
    -Wredundant-constraints

-- Library
library
  import:           common-settings
  exposed-modules:
    MyProject.Api
    MyProject.Server
    MyProject.Database
    MyProject.Auth
  other-modules:
    MyProject.Internal.Utils
  build-depends:
    base           >= 4.14 && < 5
  , servant        >= 0.20
  , servant-server >= 0.20
  , warp           >= 3.3
  , aeson          >= 2.0
  , postgresql-simple >= 0.6
  , text           >= 2.0
  , bytestring     >= 0.11
  hs-source-dirs: src
  
-- Executable
executable my-project-server
  import:        common-settings
  main-is:       Main.hs
  hs-source-dirs: app
  build-depends:
    base
  , my-project
  ghc-options: -threaded -rtsopts -with-rtsopts=-N

-- Test suite
test-suite unit-tests
  import:        common-settings
  type:          exitcode-stdio-1.0
  main-is:       Spec.hs
  hs-source-dirs: test
  build-depends:
    base
  , my-project
  , hspec       >= 2.11
  , hspec-discover
  , QuickCheck  >= 2.14
  ghc-options: -threaded -rtsopts -with-rtsopts=-N

-- Benchmark
benchmark my-benchmarks
  import:        common-settings
  type:          exitcode-stdio-1.0
  main-is:       Bench.hs
  hs-source-dirs: bench
  build-depends:
    base
  , my-project
  , criterion   >= 1.5
  ghc-options: -threaded -rtsopts -with-rtsopts=-N
```

---

## ขั้นตอนที่ 967: Stack Build System

```yaml
# stack.yaml

resolver: lts-22.6

packages:
- .
- ./my-library
- location:
    git: https://github.com/example/custom-package.git
    commit: abc123def456
  extra-dep: true

# Extra dependencies not in LTS
extra-deps:
- servant-0.20.1
- servant-server-0.20.1
- some-package-1.2.3@sha256:abc123...,12345

# GHC options
ghc-options:
  "$locals": -O2
  my-project: -Wall

# System libraries
extra-lib-dirs:
- /usr/local/lib

# Docker integration
docker:
  enable: false
  image: fpco/stack-build:lts-22

# Build flags
flags:
  my-project:
    enable-dev-mode: false
    use-postgresql: true

# Test coverage
coverage: true
```

```haskell
-- cabal.project for multi-package projects

packages: 
  .
  ./packages/api
  ./packages/client  
  ./packages/common

source-repository-package
  type: git
  location: https://github.com/example/shared-utils.git
  tag: v1.2.3
  subdir: lib

constraints:
  aeson >= 2.0,
  text >= 2.0

optimization: True

allow-newer:
  base:template-haskell

tests: True
benchmarks: True

-- Cabal.project.local (gitignore this)
-- optimization: False
-- test-show-details: streaming
```

---

## ขั้นตอนที่ 968: CI/CD with Nix

```yaml
# .github/workflows/ci.yml

name: CI

on:
  push:
    branches: [main, develop]
  pull_request:
    branches: [main]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - name: Install Nix
        uses: cachix/install-nix-action@v23
        with:
          nix_path: nixpkgs=channel:nixos-23.11
          extra_nix_config: |
            experimental-features = nix-command flakes
      
      - name: Setup Cachix
        uses: cachix/cachix-action@v13
        with:
          name: my-project
          authToken: '${{ secrets.CACHIX_AUTH_TOKEN }}'
      
      - name: Build
        run: nix build .#
      
      - name: Test
        run: nix build .#checks.x86_64-linux.my-tests
      
      - name: Lint
        run: |
          nix run .#hlint -- --error src/
          nix run .#ormolu -- --mode check $(find src -name '*.hs')
      
      - name: Build Docker Image
        if: github.ref == 'refs/heads/main'
        run: nix build .#docker
      
      - name: Push Docker Image
        if: github.ref == 'refs/heads/main'
        run: |
          docker load < result
          docker tag my-app:latest ghcr.io/${{ github.repository }}:latest
          echo "${{ secrets.GITHUB_TOKEN }}" | docker login ghcr.io -u $ --password-stdin
          docker push ghcr.io/${{ github.repository }}:latest
```

```nix
# CI checks in flake.nix
checks = {
  # Run tests
  my-tests = pkgs.runCommand "run-tests" {
    buildInputs = [ self.packages.${system}.default ];
  } ''
    cd ${self}
    ${self.packages.${system}.default}/bin/run-tests
    touch $out
  '';
  
  # Lint check
  hlint = pkgs.runCommand "hlint" {
    buildInputs = [ hsPkgs.hlint ];
  } ''
    hlint ${./src}
    touch $out
  '';
  
  # Format check
  ormolu = pkgs.runCommand "ormolu" {
    buildInputs = [ hsPkgs.ormolu ];
  } ''
    ormolu --mode check $(find ${./src} -name '*.hs')
    touch $out
  '';
};
```

---

## ขั้นตอนที่ 969: GHC Plugin Development

```haskell
-- Custom GHC Plugin

module MyPlugin (plugin) where

import GHC.Plugins

-- Plugin entry point
plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos = installPlugin
  , pluginRecompile  = flagRecompile
  }

installPlugin :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
installPlugin opts todos = do
  liftIO (putStrLn "MyPlugin: Installing...")
  return (CoreDoPluginPass "MyPass" pass : todos)

-- Core pass
pass :: ModGuts -> CoreM ModGuts
pass guts = do
  dflags <- getDynFlags
  let binds = mg_binds guts
  binds' <- mapM (transformBind dflags) binds
  return guts { mg_binds = binds' }

transformBind :: DynFlags -> CoreBind -> CoreM CoreBind
transformBind dflags (NonRec b expr) = do
  expr' <- transformExpr dflags expr
  return (NonRec b expr')
transformBind dflags (Rec pairs) = do
  pairs' <- mapM (\(b, e) -> (b,) <$> transformExpr dflags e) pairs
  return (Rec pairs')

transformExpr :: DynFlags -> CoreExpr -> CoreM CoreExpr
transformExpr dflags expr = case expr of
  -- Count function applications
  App f arg -> do
    liftIO (putStrLn ("Application: " ++ showSDoc dflags (ppr f)))
    App <$> transformExpr dflags f <*> transformExpr dflags arg
  
  Lam b body -> Lam b <$> transformExpr dflags body
  
  Let bind body -> Let <$> transformBind dflags bind <*> transformExpr dflags body
  
  Case e b ty alts -> do
    e'    <- transformExpr dflags e
    alts' <- mapM (\(Alt con bs body) -> Alt con bs <$> transformExpr dflags body) alts
    return (Case e' b ty alts')
  
  _ -> return expr
```

---

## ขั้นตอนที่ 970: Custom Preprocessors

```haskell
-- Custom GHC preprocessor

{-# OPTIONS_GHC -F -pgmF mypreprocessor #-}

-- Preprocessor source (separate executable)
module Main where

import System.Environment (getArgs)
import System.IO
import Text.Regex.PCRE

main :: IO ()
main = do
  args <- getArgs
  let [_orig, _infile, outfile] = args
  content <- hGetContents stdin
  let processed = preprocess content
  writeFile outfile processed

preprocess :: String -> String
preprocess = replaceAll patterns
  where
    patterns =
      [ ("@debug (.*)", "trace \\1 (\\1)")  -- @debug x => trace x x
      , ("@timed (.*)", "timed \"\\1\" (\\1)")
      , ("@memoize (.*)", "memo (\\1)")
      ]

-- Template preprocessor using QuasiQuotes
{-# LANGUAGE QuasiQuotes #-}

import Language.Haskell.TH.Quote

-- SQL quasi-quoter
sql :: QuasiQuoter
sql = QuasiQuoter
  { quoteExp  = parseSqlExp
  , quotePat  = error "sql: no pattern"
  , quoteType = error "sql: no type"
  , quoteDec  = error "sql: no declaration"
  }

parseSqlExp :: String -> Q Exp
parseSqlExp sqlStr = do
  let params = extractParams sqlStr
      cleaned = cleanSql sqlStr
  [| Query (T.pack cleaned) |]

-- Usage
query :: Query
query = [sql| SELECT * FROM users WHERE id = $1 AND active = true |]
```

---

## ขั้นตอนที่ 971: Cross-Compilation

```nix
# Cross-compilation with Nix

{ pkgs ? import <nixpkgs> {} }:

let
  # Cross-compile for ARM
  crossPkgs = import <nixpkgs> {
    crossSystem = {
      config      = "aarch64-unknown-linux-gnu";
      libc        = "glibc";
    };
  };
  
  # Cross-compile Haskell
  haskellCross = crossPkgs.haskellPackages.callCabal2nix "my-app" ./. {};
  
  # Cross-compile for Windows
  windowsPkgs = import <nixpkgs> {
    crossSystem = {
      config = "x86_64-w64-mingw32";
    };
  };
  
  haskellWindows = windowsPkgs.haskellPackages.callCabal2nix "my-app" ./. {};

in {
  arm64   = haskellCross;
  windows = haskellWindows;
  native  = pkgs.haskellPackages.callCabal2nix "my-app" ./. {};
}
```

```haskell
-- Cross-platform Haskell code

module Platform where

import System.Info (os, arch)

-- Platform detection
data Platform = Linux | MacOS | Windows | Unknown Text
  deriving (Show, Eq)

currentPlatform :: Platform
currentPlatform = case os of
  "linux"   -> Linux
  "darwin"  -> MacOS
  "mingw32" -> Windows
  other     -> Unknown (T.pack other)

-- Platform-specific code
getConfigDir :: IO FilePath
getConfigDir = case currentPlatform of
  Linux   -> (</> ".config/myapp") <$> getHomeDirectory
  MacOS   -> (</> "Library/Application Support/myapp") <$> getHomeDirectory
  Windows -> (</> "AppData/Roaming/myapp") <$> getHomeDirectory
  _       -> return "/etc/myapp"

-- Conditional compilation with CPP
{-# LANGUAGE CPP #-}

#ifdef linux_HOST_OS
linuxSpecific :: IO ()
linuxSpecific = putStrLn "Running on Linux"
#endif

#ifdef darwin_HOST_OS
darwinSpecific :: IO ()
darwinSpecific = putStrLn "Running on macOS"
#endif
```

---

## ขั้นตอนที่ 972: Build Reproducibility

```nix
# Reproducible builds

# Lock file for reproducibility
# flake.lock is automatically generated and committed

# Override specific packages
pkgs.haskellPackages.override {
  overrides = hfinal: hprev: {
    # Pin to specific version
    aeson = hprev.aeson_2_2_0_0;
    
    # Apply patch
    servant = pkgs.haskell.lib.appendPatches hprev.servant [
      ./patches/servant-fix.patch
    ];
    
    # Disable tests for dependency
    postgresql-simple = pkgs.haskell.lib.dontCheck hprev.postgresql-simple;
    
    # Mark as broken (exclude from build)
    broken-package = pkgs.haskell.lib.markBroken hprev.broken-package;
    
    # Build from local source
    my-lib = hprev.callCabal2nix "my-lib" ../my-lib {};
  };
}

# Hermetic builds - no network access during build
pkgs.stdenv.mkDerivation {
  name    = "hermetic-build";
  src     = ./.;
  
  # All dependencies must be in closure
  buildInputs = [ pkgs.ghc pkgs.cabal-install ];
  
  # Disable network in sandbox (default in NixOS)
  __noChroot = false;  # ensure sandbox is active
  
  buildPhase = ''
    # This will fail if any network access is attempted
    cabal build --offline
  '';
}
```

---

## ขั้นตอนที่ 973: Nix Module System

```nix
# Custom NixOS module for Haskell service

{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.myHaskellApp;
in {
  # Module options
  options.services.myHaskellApp = {
    enable = mkEnableOption "My Haskell Application";
    
    port = mkOption {
      type        = types.port;
      default     = 8080;
      description = "Port to listen on";
    };
    
    host = mkOption {
      type        = types.str;
      default     = "127.0.0.1";
      description = "Host to bind to";
    };
    
    database = {
      host = mkOption { type = types.str; default = "localhost"; };
      port = mkOption { type = types.port; default = 5432; };
      name = mkOption { type = types.str; };
      user = mkOption { type = types.str; };
      passwordFile = mkOption { type = types.nullOr types.path; default = null; };
    };
    
    logLevel = mkOption {
      type    = types.enum [ "debug" "info" "warn" "error" ];
      default = "info";
    };
    
    extraConfig = mkOption {
      type    = types.attrs;
      default = {};
    };
  };
  
  # Module implementation
  config = mkIf cfg.enable {
    systemd.services.my-haskell-app = {
      description = "My Haskell Application";
      wantedBy    = [ "multi-user.target" ];
      after       = [ "network.target" "postgresql.service" ];
      
      environment = {
        PORT       = toString cfg.port;
        HOST       = cfg.host;
        LOG_LEVEL  = cfg.logLevel;
        DB_HOST    = cfg.database.host;
        DB_PORT    = toString cfg.database.port;
        DB_NAME    = cfg.database.name;
        DB_USER    = cfg.database.user;
      };
      
      serviceConfig = {
        ExecStart    = "${pkgs.myHaskellApp}/bin/server";
        User         = "my-haskell-app";
        Group        = "my-haskell-app";
        Restart      = "on-failure";
        RestartSec   = "5s";
        # Security hardening
        NoNewPrivileges = true;
        ProtectSystem   = "strict";
        PrivateTmp      = true;
      };
    };
    
    users.users.my-haskell-app = {
      isSystemUser = true;
      group        = "my-haskell-app";
    };
    
    users.groups.my-haskell-app = {};
  };
}
```

---

## ขั้นตอนที่ 974: Docker with Nix

```nix
# Building minimal Docker images with Nix

{ pkgs ? import <nixpkgs> {} }:

let
  app = pkgs.haskellPackages.callCabal2nix "my-app" ./. {};
  
  # Minimal image with only required files
  image = pkgs.dockerTools.buildLayeredImage {
    name     = "my-haskell-app";
    tag      = "latest";
    
    contents = [
      app
      pkgs.cacert      # SSL certificates
      pkgs.tzdata      # timezone data
    ];
    
    config = {
      Cmd         = [ "${app}/bin/my-app" ];
      Env         = [ "PORT=8080" ];
      ExposedPorts = { "8080/tcp" = {}; };
      User         = "1000:1000";
      WorkingDir   = "/app";
    };
    
    # Optimize layer structure
    maxLayers = 120;
  };
  
  # Distroless-like image
  distrolessImage = pkgs.dockerTools.buildImage {
    name = "my-app-distroless";
    
    copyToRoot = pkgs.buildEnv {
      name  = "image-root";
      paths = [ app pkgs.cacert ];
      pathsToLink = [ "/bin" "/lib" "/etc" ];
    };
    
    config = {
      Cmd = [ "/bin/my-app" ];
    };
  };

in { inherit image distrolessImage; }
```

---

## ขั้นตอนที่ 975: Nix Home Manager

```nix
# home-manager configuration for Haskell developer

{ config, pkgs, ... }:

{
  home.username  = "haskell-dev";
  home.homeDirectory = "/home/haskell-dev";
  home.stateVersion  = "23.11";
  
  # Packages
  home.packages = with pkgs; [
    # Haskell tools
    ghc
    cabal-install
    stack
    haskellPackages.haskell-language-server
    haskellPackages.hlint
    haskellPackages.ormolu
    haskellPackages.ghcid
    haskellPackages.hoogle
    
    # Dev tools
    git
    ripgrep
    fzf
    jq
    postgresql
    redis
    
    # Editor
    neovim
  ];
  
  # Git config
  programs.git = {
    enable     = true;
    userName   = "Haskell Dev";
    userEmail  = "dev@example.com";
    extraConfig = {
      init.defaultBranch = "main";
      pull.rebase        = true;
    };
  };
  
  # Direnv for per-project environments
  programs.direnv = {
    enable            = true;
    nix-direnv.enable = true;
  };
  
  # .envrc for Haskell projects
  home.file.".config/direnv/lib/haskell.sh".text = ''
    use_cabal() {
      use nix
      watch_file cabal.project cabal.project.local *.cabal
    }
  '';
  
  # Shell config
  programs.bash = {
    enable = true;
    shellAliases = {
      ghci  = "ghci -interactive-print=Text.Pretty.Simple.pPrint";
      cabal = "cabal --enable-benchmarks --enable-tests";
    };
  };
}
```

---

## ขั้นตอนที่ 976: Haskell Package Management

```haskell
-- Custom Hackage server and package management

-- Registering a package on Hackage
-- 1. Create package
-- 2. Add to cabal.project or stack.yaml for testing
-- 3. Build distribution: cabal sdist
-- 4. Upload: cabal upload dist-newstyle/sdist/my-pkg-1.0.0.tar.gz

-- Package version bounds
module Compatibility where

-- Use version bounds in cabal:
-- build-depends: base >= 4.14 && < 5

-- Check compatibility at compile time
#if MIN_VERSION_base(4, 16, 0)
-- base >= 4.16
import GHC.Generics (Generic)
#else
-- base < 4.16
import GHC.Generics (Generic)
#endif

-- Testing package on different GHC versions with matrix builds
-- .github/workflows/matrix.yml
-- strategy:
--   matrix:
--     ghc: ['9.2.8', '9.4.7', '9.6.3', '9.8.1']
--     cabal: ['3.10']

-- Hackage Revision workflow
-- 1. Upload package to Hackage
-- 2. If bounds are wrong: upload cabal-file-only revision
-- 3. Revision updates metadata without touching source

-- Package documentation (Haddock)
-- | This module provides utilities for ...
-- 
-- Example usage:
-- 
-- @
-- let result = myFunction 42 "hello"
-- print result
-- @
module MyPackage.Utils
  ( -- * Main functions
    myFunction
    -- * Types
  , MyType(..)
    -- * Re-exports
  , module Data.Text
  ) where
```

---

## ขั้นตอนที่ 977: Profiling Build Configuration

```cabal
-- Profiling build configuration

-- Enable profiling builds
executable my-profiling-target
  main-is:       Main.hs
  ghc-options:
    -prof
    -fprof-auto
    -fprof-cafs
    -rtsopts
  build-depends: ...
```

```haskell
-- Runtime profiling configuration

module Profiling where

-- Run with profiling:
-- ./my-app +RTS -p -hc -hy -hd -i0.1 -RTS

-- Analyze heap profile:
-- hp2ps -c my-app.hp
-- convert profile.ps profile.pdf (using ps2pdf)

-- Cost centers
{-# SCC "heavy-computation" #-}
heavyComputation :: [Int] -> Int
heavyComputation = foldl' (+) 0

-- Ticky-ticky profiling (closure counts)
-- ghc -ticky -ticky-allocd ...
-- Run: ./my-app +RTS -r my-app.ticky -RTS
-- Analyze: ticky my-app.ticky

-- ThreadScope for parallel profiling
-- Compile: ghc -eventlog ...
-- Run: ./my-app +RTS -l -N4 -RTS
-- View: threadscope my-app.eventlog

-- GHC Core inspection
-- ghc -ddump-simpl -dsuppress-all -O2 ...

-- Assembly output
-- ghc -ddump-asm ...

-- Timing specific code
import System.Clock

timeAction :: IO a -> IO (a, Double)
timeAction action = do
  start  <- getTime Monotonic
  result <- action
  end    <- getTime Monotonic
  let elapsed = fromIntegral (toNanoSecs (diffTimeSpec end start)) / 1e9
  return (result, elapsed)
```

---

## ขั้นตอนที่ 978: Deployment Patterns

```nix
# NixOS deployment with NixOps / deploy-rs

# deploy.nix (using deploy-rs)
{
  nodes = {
    production = {
      hostname       = "prod.example.com";
      profiles.system = {
        user = "root";
        path = deploy-rs.lib.x86_64-linux.activate.nixos
          self.nixosConfigurations.production;
      };
    };
    
    staging = {
      hostname       = "staging.example.com";
      profiles.system = {
        user = "root";
        path = deploy-rs.lib.x86_64-linux.activate.nixos
          self.nixosConfigurations.staging;
      };
    };
  };
  
  # Deployment checks
  checks = builtins.mapAttrs (name: cfg: cfg.config.system.build.toplevel)
    self.nixosConfigurations;
}
```

```bash
# Deployment commands
# Deploy to production
deploy .#production

# Rollback to previous generation
nixos-rebuild --rollback switch

# Check current system
nixos-version
nix-store --query --roots /run/current-system

# Build without deploying
nix build .#nixosConfigurations.production.config.system.build.toplevel

# Test in VM
nix build .#nixosConfigurations.production.config.system.build.vm
./result/bin/run-*-vm
```

---

## ขั้นตอนที่ 979: Package Registry

```haskell
-- Internal package registry

module Registry where

import Data.Map.Strict (Map)
import Network.HTTP.Types
import Servant

-- Internal Hackage-like registry API
type RegistryAPI
  = "packages" :> Get '[JSON] [PackageInfo]
  :<|> "packages" :> Capture "name" Text :> Get '[JSON] PackageInfo
  :<|> "packages" :> Capture "name" Text :> Capture "version" Text :> Get '[JSON] PackageRelease
  :<|> "packages" :> Capture "name" Text :> Capture "version" Text :> "tarball" :> Get '[OctetStream] BS.ByteString
  :<|> "upload" :> MultipartForm Mem MultipartData :> Post '[JSON] UploadResult

data PackageInfo = PackageInfo
  { pkgName     :: Text
  , pkgVersions :: [Text]
  , pkgLatest   :: Text
  , pkgSynopsis :: Text
  , pkgLicense  :: Text
} deriving (Generic, ToJSON, FromJSON)

data PackageRelease = PackageRelease
  { relName       :: Text
  , relVersion    :: Text
  , relCabalFile  :: Text
  , relTarballUrl :: Text
  , relUploadedAt :: UTCTime
} deriving (Generic, ToJSON, FromJSON)

-- Registry server
registryServer :: Registry -> Server RegistryAPI
registryServer reg =
  listPackages reg
  :<|> getPackage reg
  :<|> getRelease reg
  :<|> downloadTarball reg
  :<|> uploadPackage reg

-- Add custom registry to cabal.project
-- In cabal.project:
-- repository my-registry
--   url: https://registry.internal.example.com
--   secure: True
--   root-keys: abc123...
--   key-threshold: 1
```

---

## ขั้นตอนที่ 980: โปรเจกต์: Complete Nix-Based Haskell Infrastructure

```nix
# Complete production-grade Nix infrastructure

{
  description = "Production Haskell Application Infrastructure";
  
  inputs = {
    nixpkgs.url     = "github:NixOS/nixpkgs/nixos-23.11";
    flake-utils.url = "github:numtide/flake-utils";
    deploy-rs.url   = "github:serokell/deploy-rs";
    
    haskell-nix = {
      url    = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  
  outputs = { self, nixpkgs, flake-utils, deploy-rs, haskell-nix, ... }@inputs:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ];
    in
    flake-utils.lib.eachSystem supportedSystems (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ haskell-nix.overlay ];
        };
        
        project = pkgs.haskell-nix.project {
          src = ./.;
          compiler-nix-name = "ghc965";
          shell.tools = {
            cabal                   = "latest";
            haskell-language-server = "latest";
            hlint                   = "latest";
            ormolu                  = "latest";
            ghcid                   = "latest";
          };
        };
        
      in {
        packages = {
          default = project.my-app.components.exes.my-app-server;
          
          docker = pkgs.dockerTools.buildLayeredImage {
            name     = "my-app";
            contents = [ project.my-app.components.exes.my-app-server pkgs.cacert ];
            config.Cmd = [ "/bin/my-app-server" ];
          };
        };
        
        devShells.default = project.shellFor {
          withHoogle = true;
          exactDeps  = true;
        };
        
        checks = {
          tests = project.my-app.components.tests.my-tests;
          
          formatting = pkgs.runCommand "check-format" {
            buildInputs = [ pkgs.haskellPackages.ormolu ];
          } ''
            ormolu --mode check ${./src}/**/*.hs && touch $out
          '';
        };
      }
    )
    // {
      nixosConfigurations.production = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./nixos/production.nix
          {
            environment.systemPackages = [
              self.packages.x86_64-linux.default
            ];
          }
        ];
      };
      
      deploy.nodes.production = {
        hostname = "prod.example.com";
        profiles.system = {
          user = "root";
          path = deploy-rs.lib.x86_64-linux.activate.nixos
                   self.nixosConfigurations.production;
        };
      };
    };
}
```

---

*[← Part 48](part-48.md) | [Part 50 →](part-50.md)*
