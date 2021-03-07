{ name = "halogen-project"
, dependencies =
  [ "affjax", "console", "effect", "halogen", "psci-support", "random" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
