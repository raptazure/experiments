defmodule SchoolBasics.MixProject do
  use Mix.Project

  def project do
    [
      app: :school_basics,
      version: "0.1.0",
      elixir: "~> 1.10",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  def deps do
    [{:earmark, "~> 1.2", only: :dev}, {:ex_doc, "~> 0.19", only: :dev}, {:tzdata, "~> 1.0.4"}]
  end
end
