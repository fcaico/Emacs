/*
 * Copyright (c) 2000, 2001    Paul Kinnucan
 *
 * $Revision: 1.1 $
 */

package jde.debugger.command;
import jde.debugger.JDEException;


/**
 * List all threads. 'get_threads' command.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * get_threads
 * </pre>
 *
 * <b>Returns:</b>
 * <pre>
 * (jde-dbo-command-result cmd_id {@link #getAllThreadsInformation thread-info})
 * </pre>
 *
 * <b>Comments:</b>
 * <ul>
 * <li> There are a couple of quirks regarding the reporting
 * of thread state:
 * <ul>
 * <li> Quirk 1: Due to a bug in ThreadReference.isAtBreakpoint(),
 *        a thread will report a breakpoint at a location
 *	      even if a threadFilter has been applied for the
 *	      thread. ie, if test.Test.java:41 is your
 *	      breakpoint, and you've applied a threadfilter
 *	      saying you DON'T want an event if the thread ==
 *	      Thread-A, and you somehow suspend Thread-A at
 *	      exactly that line, and do a 'get_threads';
 *	      Thread-A will report to be suspended on a
 *	      breakpoint, although ordinarily it would have
 *	      skipped it.
 *
 * <li> Quirk 2: There seems to be no way of reporting the
 *        status if the user does a
 *	      Thread.suspend(). Well, it's deprecated
 *	      anyways... *shrug*.
 * </ul>
 * </ul>
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.1 $
 *
 */
public class GetThreads extends DebugProcessCommand {
  
  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {
    jde.signalCommandResult(procID, cmdID, proc.getAllThreadsInformation());
  }



  public Object clone() {return new GetThreads();}
  
} // GetThreads

/*
 * $Log: GetThreads.java,v $
 * Revision 1.1  2001/03/24 13:35:25  paulk
 * Initial revision.
 *
 *
 */

// End of GetThreads.java
