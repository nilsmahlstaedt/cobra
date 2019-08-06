package net.flatmap.cobra.util

object PID {
  def get(): Long = {
    import java.lang.management.ManagementFactory
    import java.lang.management.RuntimeMXBean
    val bean = ManagementFactory.getRuntimeMXBean

    // Get name representing the running Java virtual machine.
    // It returns something like 6460@AURORA. Where the value
    // before the @ symbol is the PID.
    val jvmName = bean.getName
    System.out.println("Name = " + jvmName)

    // Extract the PID by splitting the string returned by the
    // bean.getName() method.
    val pid = jvmName.split("@")(0).toLong
    System.out.println("PID  = " + pid)
    pid
  }
}
