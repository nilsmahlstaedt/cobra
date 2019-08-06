package net.flatmap.cobra.util

import java.util.concurrent.CompletableFuture

import org.eclipse.lsp4j.jsonrpc.messages
import scala.jdk.CollectionConverters._

trait LSConverters {

  implicit class ConvertableEither[A, B](e: messages.Either[A, B]) {
    def toScala: scala.Either[A, B] = if (e.isLeft) {
      Left(e.getLeft)
    } else {
      Right(e.getRight)
    }
  }

  implicit class ConvertableResponse[A, B](v: CompletableFuture[java.util.List[messages.Either[A, B]]]) {
    def convertToScala: List[Either[A, B]] = v.get.asScala.map(_.toScala).toList
  }

}

object LSConverters extends LSConverters
