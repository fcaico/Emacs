/*
 * Copyright (c) 2000, 2001    Paul Kinnucan
 *
 * $Revision: 1.1 $
 */

package jde.debugger.command;
import jde.debugger.JDEException;
import jde.debugger.Etc;



/**
 * 'clear' command. Clears a breakpoint, watchpoint or an exception
 * intercept
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * clear specID
 * </pre>
 *
 * <b>Comments:</b>
 * <ul>
 * <li> specIDs are returned in the 'break'/'watch'/'trace_exceptions'
 * commands.
 * </ul>
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.1 $
 *
 */
public class Clear extends DebugProcessCommand {
  
  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {
    if (args.size() < 1)
      throw new JDEException("Insufficient arguments");
    Long specID = Etc.safeGetLong(args.remove(0), "spec ID");
    proc.getEventRequestSpecs().removeSpec(specID);
    jde.signalCommandResult(procID, cmdID);  }

  public Object clone() {return new Clear();}
  
} // Clear

/*
 * $Log: Clear.java,v $
 * Revision 1.1  2001/03/24 05:48:40  paulk
 * Initial version.
 *
 *
 */

// End of Clear.java
