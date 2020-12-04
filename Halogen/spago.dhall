{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "halogen-midi-keyboard"
, dependencies =
  [ "aff-coroutines", "console", "effect", "halogen", "psci-support",
    "midi", "soundfonts" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
