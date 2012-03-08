/*
 * Copyright (c) 2000, 2001    Paul Kinnucan
 *
 * $Revision: 1.1 $
 */

package jde.debugger.command;
import jde.debugger.JDEException;


/**
 * 'finish' command.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * finish
 * </pre>
 *
 * <b>Comments:</b>
 * <ul>
 * <li> if multiple VMs are being debugged, this command will
 * kill the one corresponding to app_id, retaining others.
 * </ul>
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.1 $
 *
 */
public class Finish extends DebugProcessCommand {
  
  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {
    proc.shutdown();
    jde.signalCommandResult(procID, cmdID);
  }

  public Object clone() {return new Finish();}
  
} // Finish

/*
 * $Log: Finish.java,v $
 * Revision 1.1  2001/03/24 05:52:12  paulk
 * Initial version.
 *
 *
 */

// End of Finish.java
