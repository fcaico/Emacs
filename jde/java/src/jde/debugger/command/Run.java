/*
 * Copyright (c) 2000, 2001    Paul Kinnucan
 *
 * $Revision: 1.2 $
 */

package jde.debugger.command;
import jde.debugger.JDEException;


/**
 * Runs an application.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * run
 * </pre>
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.2 $
 */
public class Run extends DebugProcessCommand {
  
  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {

    try {
      proc.getVM().resume();
    } catch (Exception ex) {
      throw new JDEException("Unspecified Error occured: "+ex.toString());
    }
    jde.signalCommandResult(procID, cmdID);
 
  }

  public Object clone() {return new Run();}
  
} // Run

/*
 * $Log: Run.java,v $
 * Revision 1.2  2001/03/24 05:42:37  paulk
 * Updated to reflect reorganization of debugger code.
 *
 * Revision 1.1  2000/01/31 12:48:02  paulk
 * Start or continue application.
 *
 */

// End of Run.java
