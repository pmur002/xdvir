
library(xdvir)

author("This is a test: $x - \\mu$")

## Invalid engine
tools::assertError(author("test", engine=NULL))

## Explicit engine
author("test", engine=latex:::nullEngine)

## Engine by name
author("test", engine="null")

## Unknown engine (name)
tools::assertError(author("test", engine="unknown"))
