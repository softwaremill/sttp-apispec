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
"com.softwaremill.sttp.apispec" %% "apispec-model" % "0.3.0"

// only model classes, root: OpenAPI
"com.softwaremill.sttp.apispec" %% "openapi-model" % "0.3.0"
// circe encoders for the model classes
"com.softwaremill.sttp.apispec" %% "openapi-circe" % "0.3.0"
// extension method for OpenAPI to convert to yaml
"com.softwaremill.sttp.apispec" %% "openapi-circe-yaml" % "0.3.0"

// only model classes, root: AsyncAPI
"com.softwaremill.sttp.apispec" %% "asyncapi-model" % "0.3.0"
// circe encoders for the model classes
"com.softwaremill.sttp.apispec" %% "asyncapi-circe" % "0.3.0"
// extension method for AsyncAPI to convert to yaml
"com.softwaremill.sttp.apispec" %% "asyncapi-circe-yaml" % "0.3.0"
```

sttp apispec is available for Scala 2.12, 2.13, 3, Scala.JS and Scala Native.

## Documentation

[Javadocs](https://www.javadoc.io/doc/com.softwaremill.sttp.apispec/apispec-model_2.12/latest/sttp/apispec/index.html)

## Usage

Sttp-apispec defines models and encoding/decoding logic for OpenAPI / AsyncAPI documentation.
While for the most part sttp-apispec is meant to be used with [tapir](https://github.com/softwaremill/tapir) 
(where tapir converts endpoint definitions to [OpenAPI](https://tapir.softwaremill.com/en/latest/docs/openapi.html)/[AsyncAPI](https://tapir.softwaremill.com/en/latest/docs/asyncapi.html) model, 
and then sttp-apispec encodes it to desired documentation file) it can be used as a stand-alone as well in the following way:

### Open API
Currently, OpenAPI versions 3.0.3 and 3.1 are supported, where the models are the same, 
and choosing one version over the other is a matter of selecting the proper encoder.

Add dependencies:
```scala
"com.softwaremill.sttp.apispec" %% "openapi-model" % "@VERSION@"
"com.softwaremill.sttp.apispec" %% "openapi-circe-yaml" % "@VERSION@"
```

Create example OpenAPI models:
```scala
import sttp.apispec.openapi._

import scala.collection.immutable.ListMap

val openApi3_0 = OpenAPI(
  openapi = "3.0.3",
  info = Info(title = "Fruits", version = "1.0"),
  paths = Paths(ListMap("/" ->
    PathItem(get = Some(Operation(
      operationId = Some("getRoot"),
      responses = Responses(ListMap(ResponsesCodeKey(200) -> Right(Response(description = "")))))))))
) // define example OpenAPI 3.0.3 model

val openApi3_1 = openApi3_0.openapi("3.1.0") // and example OpenAPI 3.1.0 model
```

Encode it to the Open API YAML string:
```scala
import sttp.apispec.openapi.circe.yaml._

val yaml = openApi3_0.toYaml // converts model to OpenApi 3.0.3 yaml string
val yaml3_1 = openApi3_1.toYaml3_1 // converts model to OpenApi 3.1.0 yaml string
```

### Async API
Currently, AsyncAPI version 2.0 is supported. 
Usage is the following:

Add dependencies
```scala
"com.softwaremill.sttp.apispec" %% "asyncapi-model" % "@VERSION@"
"com.softwaremill.sttp.apispec" %% "asyncapi-circe-yaml" % "@VERSION@"
```

Create an example AsyncAPI model:
```scala
import sttp.apispec.asyncapi._
import sttp.apispec.{Schema, SchemaType}
import scala.collection.immutable.ListMap

val asyncApi = AsyncAPI(
  asyncapi = "2.0.0",
  id = None,
  info = Info(title = "Fruits", version = "1.0"),
  servers = ListMap.empty,
  channels = ListMap("/" -> Right(ChannelItem(
    description = None,
    subscribe = Some(Operation(
      operationId = Some("onRoot"),
      summary = None,
      description = None,
      tags = List.empty,
      externalDocs = None,
      bindings = List.empty,
      traits = List.empty,
      message = Some(Right(SingleMessage(payload = Some(Right(Right(Schema(SchemaType.Integer))))))),
    )),
    publish = None,
    parameters = ListMap.empty,
    bindings = List.empty
  ))),
  components = None,
  tags = List.empty,
  externalDocs = None
) //define example AsyncApi 2.0.0 model
```

Encode it to Async API YAML string:
```scala
import sttp.apispec.asyncapi.circe.yaml._

val yaml2_0 = asyncApi.toYaml // // convert model to AsyncApi 2.0.0 yaml string
```

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
