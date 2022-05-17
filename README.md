![sttp-apispec](https://github.com/softwaremill/sttp-apispec/raw/master/banner.png)

[![Join the chat at https://gitter.im/softwaremill/tapir](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/softwaremill/tapir)
[![CI](https://github.com/softwaremill/sttp-apispec/workflows/CI/badge.svg)](https://github.com/softwaremill/sttp-apispec/actions?query=workflow%3ACI+branch%3Amaster)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.softwaremill.sttp.apispec/apispec-model_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.softwaremill.sttp.apispec/apispec-model_2.13)

sttp is a family of Scala HTTP-related projects, and currently includes:

* [sttp client](https://github.com/softwaremill/sttp): The Scala HTTP client you always wanted!
* [sttp tapir](https://github.com/softwaremill/tapir): Typed API descRiptions.
* [sttp model](https://github.com/softwaremill/sttp-model): Simple Scala HTTP model.
* sttp apispec: this project. OpenAPI, AsyncAPI and JSON Schema models.

## Quickstart with sbt

Add one of the following dependencies:

```scala
// common model classes, including Schema
"com.softwaremill.sttp.apispec" %% "apispec-model" % "0.1.0"

// only model classes, root: OpenAPI
"com.softwaremill.sttp.apispec" %% "openapi-model" % "0.1.0"
// circe encoders for the model classes
"com.softwaremill.sttp.apispec" %% "openapi-circe" % "0.1.0"

// only model classes, root: AsyncAPI
"com.softwaremill.sttp.apispec" %% "asyncapi-model" % "0.1.0"
// circe encoders for the model classes
"com.softwaremill.sttp.apispec" %% "asyncapi-circe" % "0.1.0" 
```

sttp apispec is available for Scala 2.12, 2.13, 3, Scala.JS and Scala Native.

## Converting model classes to yaml

Model classes encoded using Circe encoders can then be serialized to JSON by adding a dependency:

```scala
"io.circe" %% "circe-yaml" % "0.14.1"
```

And running the following:

```scala
import io.circe.syntax._
import io.circe.yaml.Printer

def toYaml: String = Printer(dropNullKeys = true, preserveOrder = true).pretty(myModel.asJson)
```

## Documentation

[Javadocs](https://www.javadoc.io/doc/com.softwaremill.sttp.apispec/apispec-model_2.12/latest/sttp/apispec/index.html)

## Contributing

If you have a question, or hit a problem, feel free to ask on our [gitter channel](https://gitter.im/softwaremill/tapir)!

Or, if you encounter a bug, something is unclear in the code or documentation, don’t hesitate and open an issue on GitHub.

### Building & testing the scala-native version

By default, native projects will **not** be included in the aggregate build of the root project. To include it, define the `STTP_NATIVE` environmental variable before running sbt, e.g.:

```
STTP_NATIVE=1 sbt
```

You might need to install some additional libraries, see the [scala native](http://www.scala-native.org/en/latest/user/setup.html) documentation site.

## Commercial Support

We offer commercial support for sttp and related technologies, as well as development services. [Contact us](https://softwaremill.com) to learn more about our offer!

## Copyright

Copyright (C) 2022 SoftwareMill [https://softwaremill.com](https://softwaremill.com).
