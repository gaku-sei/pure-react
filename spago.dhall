{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "affjax"
    , "console"
    , "effect"
    , "generics-rep"
    , "halogen"
    , "halogen-formless"
    , "psci-support"
    , "react-basic-hooks"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
