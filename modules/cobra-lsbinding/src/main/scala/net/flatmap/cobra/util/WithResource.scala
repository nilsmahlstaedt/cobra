package net.flatmap.cobra.util

/**
  * rebuild of java try-with-resource mechanism
  * more precisely the closing after use or failure part.
  * Exception handling still lies in the domain of the caller!
  *
  * this class is meant as a band-aid until this project can switch to scala 2.13!
  * After that point consider switching to 'using'
  * https://scala-lang.org/files/archive/api/2.13.0/scala/util/Using$.html
  */
object WithResource {

  /**
    * executes block with closable resource 'closable'
    * closing a after completing block or running into an exception
    *
    * Exceptions are NOT caught!
    * @param closable resource to be used in block
    * @param block code using resource
    * @return result of executing block with resource closable
    */
  def withResource[A <: AutoCloseable, B](closable: A)(block: A => B): B = {
    try{
      block(closable)
    }finally{
      closable.close()
    }
  }
}
