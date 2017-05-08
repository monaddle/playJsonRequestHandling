package controllers


import play.api._
import play.api.mvc._
import play.libs.Json._
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import cats.implicits._
import play.api.data.validation.ValidationError
import play.api.http.ContentTypes
import play.api.http.Status.UNSUPPORTED_MEDIA_TYPE

import scala.concurrent.ExecutionContext.Implicits.global



class Application extends Controller {

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

  // We've been tasked with the very important job of building an endpoint to store
  // transactions at a register. A transaction consists of an item name, and a cost.

  // Let's make a first stab at it: we'll take the request body, convert it to a string, and we send it to the database.
  // On it's face this has some problems, but let's enumerate them:

  // 1. we're not doing any validation on the string to see if it is actually json. 2.
  // if it is json, we're not doing any validation on its contents. 3. this actually
  // blows up if the client sends "Content-Type: application/json",
  // because request.body.asText depends on "Content-Type: text/plain" - when we try to
  // access the value within the Option[String] with maybeText.get, an exception occurs.

  // In scala, `Option[String]` is a type which has two possible states: Either it has a string, or it has no value.
  // It's analogous to `null`, except with some important improvements. Rather than just blowing up when you
  // try to access the value like `null` does, Option implements a set of functions (map, flatMap,
  // fold, filter) that allow you to conditionally run computations on the string inside of the option.
  //
  // Unfortunately, our v1 doesn't use any of those: it just tries to access the value directly, and throws
  // an exception if that value isn't present. `request.body.asText` will only return text if the Content-Type
  // specified in the request header is text/plain.

  // good curl:
  //    curl -XPOST -H "Content-Type: text/plain" -d "{\"item\":\"bike\", \"cost\":5.00}" localhost:9000/api/store/v1
  // curl that fails with a 500 error:
  //    curl -XPOST -H "Content-Type: application/json" -d "{\"item\":\"bike\", \"cost\":5.00}" localhost:9000/api/store/v1
  // curl that succeeds, but sends garbage to the database:
  //    curl -XPOST -H "Content-Type: text/plain" -d "little bobby tables starts his first job" localhost:9000/api/store/v1

  def storeTransaction_v1: Action[AnyContent] = Action {
    request =>
      val maybeText: Option[String] = request.body.asText
      DAL.storeString(maybeText.get)
      Ok("")
  }

  // To fix the first glaring issue: we want to check if the request is JSON, so we should check the body for JSON instead of text.

  // curl that succeeds:
  //    curl -XPOST -H "Content-Type: application/json" -d "{\"item\":\"bike\", \"cost\":5.00}" localhost:9000/api/store/v2

  // curl that sends an appropriate 400 error for invalid JSON:
  //    curl -XPOST -H "Content-Type: application/json" -d "bad json {\"item\":\"bike\", \"cost\":5.00}" -v localhost:9000/api/store/v2

  // curl that fails with a 500 error on `maybeJson.get`:
  //    curl -XPOST -H "Content-Type: text/plain" -d "{\"item\":\"bike\", \"cost\":5.00}" localhost:9000/api/store/v2


  def storeTransaction_v2: Action[AnyContent] = Action {
    request =>
      // Still not great - if it receives the wrong Content-Type it returns a 500 error.
      // Plus, we're not handling any failure that might occur when interacting with the datastore.
      val maybeJson: Option[JsValue] = request.body.asJson
      DAL.storeJson(maybeJson.get)
      Ok("")
  }

  // Now we want to stop the endpoint from returning a 500 error if it receives the wrong
  // content-type. To do that, we need to use some of the functions defined on Map.

  // Here we introduce two functions on Option: map and getOrElse. Option.map is a function
  // which accepts a function, and conditionally applies that function against the contents of
  // the Option. If there is a value in the Option, the function is run.
  // If the Option is empty, it performs no operation. Lest you think there's any magic going on, here's the full
  // definion of Option.map:
  // `final def map[B](f: A => B): Option[B] =
  //    if (isEmpty) None else Some(f(this.get))`
  // where A is the type of the option (e.g. String for an Option[String], and B is whichever type the function returns.
  //
  // getOrElse returns either the contents of the Option, or if the option isEmpty, it returns a default value which you passed in.
  //  `final def getOrElse[B >: A](default: => B): B =
  //    if (isEmpty) default else this.get`
  //
  // The full definition of Option is available here, in all of it's glorious simplicity:
  // https://github.com/scala/scala/blob/v2.12.2/src/library/scala/Option.scala#L1

  def storeTransaction_v3: Action[AnyContent] = Action {
    request =>
      val maybeJson: Option[JsValue] = request.body.asJson
      // here we map over the JSON - we store it in the database, and return an Ok response, converting
      // the Option[JsValue] into an Option[Result].
      val maybeOk: Option[Result] = maybeJson map {
        json: JsValue =>
          // Still, we're not checking if the JSON even has the right fields before we try to store it.
        DAL.storeJson(json)
          Ok("")
      }

      maybeOk getOrElse { BadRequest("") }
  }

  // In v3 we're letting the client know if there's invalid JSON, or if there's an invalid Content-Type,
  // but we're still not checking if the JSON has any of the values we care about. Let's put together
  // a datatype that represents a transaction:

  case class Transaction(item: String, cost: Double)

  // Case classes are scala's go-to way of representing data. While they're concise, at compile time
  // a number of functions are automatically generated - a default constructor, an apply and unapply
  // function, as well as equality operations. Instantiating one is simple:
  // `val transaction = Transaction("bike", 5.0)

  // In v4, we'll create an instance of Transaction by extracting the fields we want from
  // the json object, and then store that.
  def storeTransaction_v4: Action[AnyContent] = Action {
    request =>

      val maybeJson: Option[JsValue] = request.body.asJson
      maybeJson map {
        json: JsValue =>
          // This is much safer now: if there is valid JSON, we extract the values we want, store them in a
          // the Transaction case class, and then send them onto the datastore. However, this will blow up with a 500 error
          // if either the item or the cost aren't present in the JSON payload.
          DAL.storeTransaction(
            Transaction((json \ "item").as[String], (json \ "cost").as[Double])
          )
          Ok("")
      } getOrElse BadRequest("")
  }


  // It's kind of silly to have to name the fields we want to extract from the JSON ("item", "name") when we already
  // defined them in the case class. It would be nicer and less error-prone to only name them once:

  // Enter Play's json library. We can define a converter called a Reads[Transaction].
  // The Reads[Transaction] will get unpacked at compile time via macros to become code
  // that will canf JSON and convert it to a JsResult[Transaction] object. JsResult[Transaction] has two possible types -
  // JsSuccess[Transaction] or JsError. We can then pattern match against those types
  // and in the case of success, store the transaction, and failure, send an error message back.

  implicit def transactionReads: Reads[Transaction] = Json.reads[Transaction]

  def storeTransaction_v5: Action[AnyContent] = Action {
    request =>
      val maybeJson: Option[JsValue] = request.body.asJson
      maybeJson map {
        json =>
          val jsResult = json.validate[Transaction]
          jsResult match {
            case JsSuccess(transaction, _) => DAL.storeTransaction(transaction)
              Ok("")
            case e: JsError => BadRequest(e.toString)
          }
      } getOrElse BadRequest("")
  }

  // v5 is much better - we validate the input, only store it if it's correct, and send a 400 error
  // on incorrect input. Still, we have some remaining areas for improvement:

  // 1. It's a little silly to have to handle checking if we were passed JSON inside every controller that expects JSON.
  // Preferably we could abstract that out. it would be nice to have a concise pattern to say "Parse this request as JSON.
  // If it's not json, return an error. If it succeeds, perform these actions with that json.

  // 2. Not only are we expecting JSON, we know exactly what JSON we're expecting. It would be even nicer to
  // be have a pattern that allows us to say: "Parse this JSON request as case class Transaction. If that fails, return
  // a parse error. If that succeeds, perform these actions with that transaction."

  // 3. Also, our parse errors are html. For a JSON API, that's pretty busted. It would be nicer still if
  // we returned JSON in response to parse errors.

  // 4. We're still not handling cases where the database operation fails.

  // Play provides a built in means to handle issue #1 - body parsers. Instead of returning a body of
  // AnyContent, we can see that the body is now a JsValue. However, if invalid JSON is passed in, the
  // controller body is never invoked - an error response is sent back to the client. Unfortunately,
  // we haven't seen a way to coerce the error response into JSON - it defaults to HTML, and as far
  // as we've seen, we have to write a custom parser. Fortunately that's not particularly difficult.

  // Here, we solve issue #1 - we're passing a tolerant json parser into the action - this allows us to stop mapping over
  // the "maybeJson" value.
  def storeTransaction_v6: Action[JsValue] = Action(parse.json) {
    request =>
      val json: JsValue = request.body
      val jsResult = json.validate[Transaction]
      jsResult match {
        case s: JsSuccess[Transaction] => DAL.storeTransaction(s.value)
          Ok("")
        case e: JsError => BadRequest(e.toString)
      }
  }

  // To solve issue #2, validating against arbitrary case classes, we can write a custom BodyParser that
  // accepts the Reads[Transaction] function we defined, and validates with it. If it fails, we
  // return a BadRequest. If it succeeds, execution continues into the controller block.
  def validateJson[A: Reads]: BodyParser[A] = BodyParsers.parse.json.validate {
    _.validate[A].asEither.leftMap(e => BadRequest(JsError.toJson(e)))
  }

  // To solve issue #4, we need to start checking the results of DAL.storeTransaction.
  def storeTransaction_v7: Action[Transaction] = Action(validateJson(transactionReads)) {
    request =>
      // this code only executes if the request is a valid transaction object. It gets the Request[Transaction], whose
      // body is a Transaction object,
      // then passes the Transaction into the DAL. If storing the object succeeds, we return an empty 200 response. If
      // it fails, we return a response saying there was an issue with the database. Ta da!
      val body: Transaction = request.body
      DAL.storeTransaction(body) match {
        case Left(_) => BadRequest("There was a problem with the database...")
        case Right(_) => Ok("")
      }
  }


  // Still, there's a little bit more we can abstract out. We don't really want to recreate our error-handling
  // logic in each controller endpoint. Because
  // essentially every controller has the general pattern of - receive a request, perform a network operation (which
  // may potentially fail), and then return a result, we can centralize how we handle those potential failures.

  // One solution is that instead of returning a Result at the end of our controller blocks, we return an Either[Error, Result],
  // and then implicitly convert that to a Result. This gives us flexibility - if controller simply wants to report an error,
  // it can return a left error, the sort of thing that will come from our (very carefully designed) DAL methods. If the controller
  // wants to send some specific result, it can simply specify that in a Right(Result).

  // Implicits, when not used carefully, are spooky action at a distance. We're toeing the line here.
  // Anywhere where this implicit is in scope, it will convert a value: Either[Error, Result] to
  // a Result, by applying the function block to it.
  implicit def eitherToResult(either: Either[Error, Result]): Result = {
    either match {
      case Left(e) => BadRequest("")
      case Right(result) => result
    }
  }

  // Here we arrive at a concise, safe (at least insofar as we trust our DAL not to throw exceptions)
  // and (mostly) explicit endpoint: We validate the JSON using our transactionReads,
  // report failure if it's invalid, store it in the database if it's valid. The database operation returns
  // an Either[Error, Unit] we then map over, creating an Either[Error, Result]. This gets converted implicitly
  // into a Result, and sent back to the client.
  def storeTransaction_v8: Action[Transaction] = Action(validateJson(transactionReads)) {
    r => DAL.storeTransaction(r.body) map { _ => Ok("") }
  }

  // If we wanted to be more explicit about converting the Either[Error, Result] to a Result, we could use
  // function composition to combine our block with eitherToResult. The type signature of our controller block is
  // Request[Transaction] => Either[Error, Result]. The type signature of eitherToResult is Either[Error, Result].
  // You can imagine chaining these together: Request[Transaction] => Either[Error, Result] => Result. In scala,
  // we can combine our two functions to create a single function with the type signature Request[Transaction] => Result, using the
  // andThen function. This is called function composition. An example:
  def storeTransaction_v8_alternative: Action[Transaction] = Action(validateJson(transactionReads))({
    r: Request[Transaction] => DAL.storeTransaction(r.body) map { _ => Ok("") }
  } andThen eitherToResult)

  // Note that we had to add the type annotation Request[Transaction] to the request - when we're not explicit
  // that the request is a Request[Transaction], the compiler can't figure out the type for the function composition.
  // This leaves me slightly preferring the implicit conversion over function composition, but it's not clear
  // that either one is superior.

  // Let's say we want to add additional validation rules - maybe we want to enforce a minimum cost (maybe no negative costs)
  // and a minimum and maximum length for an item name. We can do that by defining another Reads[Transaction] that
  // enforces those validations.

  val transactionReads2: Reads[Transaction] = (
    (JsPath \ "item").read[String](minLength[String](1) keepAnd maxLength[String](500)) and
      (JsPath \ "cost").read[Double](min(0.0))
    )(Transaction.apply _)

  def storeTransaction_v9: Action[Transaction] = Action(validateJson(transactionReads2)) {
    r => DAL.storeTransaction(r.body) map { _ => Ok("") }
  }

  object DAL {
    def storeString(json: String): Either[Error, Unit] = {
      println(json)
      Right(Unit)
    }
    def storeJson(json: JsValue): Either[Error, Unit] = Right(Unit)
    def storeTransaction(transaction: Transaction): Either[Error, Unit] = Right(Unit)
  }
}

