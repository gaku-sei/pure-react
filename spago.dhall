{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "aff"
    , "affjax"
    , "concur-react"
    , "console"
    , "effect"
    , "foreign-generic"
    , "generics-rep"
    , "psci-support"
    , "react-basic-hooks"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
