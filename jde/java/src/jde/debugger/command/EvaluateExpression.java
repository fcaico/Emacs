/*
 * Copyright (c) 2000, 2001    Paul Kinnucan
 *
 * $Revision: 1.2 $
 */

package jde.debugger.command;

import com.sun.jdi.AbsentInformationException;
import com.sun.jdi.IncompatibleThreadStateException;
import com.sun.jdi.NativeMethodException;
import com.sun.jdi.ObjectCollectedException;
import com.sun.jdi.ObjectReference;
import com.sun.jdi.StackFrame;
import com.sun.jdi.ThreadReference;
import com.sun.jdi.Value;
import jde.debugger.Etc;
import jde.debugger.JDEException;
import jde.debugger.LispForm;
import jde.debugger.Rep;

    /**
     * 'evaluate' command.
     * <p>
     *
     * <b>Syntax:</b>
     * <pre>
     * evaluate threadID stackFrameIndex "expression"
     * </pre>
     *
     * <b>Returns:</b>
     * <pre>
     * (jde-dbo-command-result cmd_id {@link Rep#getValueRep(Value, ObjectStore) value})
     * </pre>
     *
     * <b>Comments:</b>
     * <ul>
     * <li> Note that stackFrameIndex = 0 corresponds to the
     * current stackframe.
     * <li> The threadID and stackFrameIndex can be got from the
     *	'get_threads' command. Note that many evaluations
     *  might not be possible depending on the state of the thread
     * </ul>
     */
 public class EvaluateExpression extends DebugProcessCommand {
  
  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
   public void doCommand() throws JDEException {

     // we'll suspend the thread later on, but wanna resume it later
     // so keep track.
     boolean weSuspendedThread = false;

     if (args.size() < 3) 
       throw new JDEException("Insufficient arguments");

     ThreadReference tRef = null;

     try {
	    
       // need a valid thread to work in...
       Long uniqueID = Etc.safeGetLong(args.remove(0), "thread ID");
       int frameIndex = Etc.safeGetint(args.remove(0), "frame index");
		    
       Object oRef = proc.getStore().get(uniqueID);
       if (oRef == null) {
	 throw new JDEException("No such thread exists");
       } else if (!(oRef instanceof ThreadReference)) {
	 throw new JDEException("Object is not a thread");
       }
	    
       tRef = (ThreadReference)oRef;

       // suspend it on our own so that nothing funny happens during
       // the time we're trying to do expression evaluation
       tRef.suspend();
       weSuspendedThread = true;
	    
       StackFrame frame = null;
       try {
	 frame = tRef.frame(frameIndex);
       } catch (IncompatibleThreadStateException ex) {
	 throw new JDEException("Thread is not suspended");
       } catch (IndexOutOfBoundsException ex) {
	 throw new JDEException("Invalid frame");
       } catch (ObjectCollectedException ex) {
	 throw new JDEException("The frame has already been garbage collected");
       }
       if (frame == null) {
	 throw new JDEException("Error ascertaining frame");
       }

       String expr = args.remove(0).toString();
       Value val = Etc.evaluate(expr, frame);
	    
       jde.signalCommandResult(procID, cmdID, Rep.getValueRep(val, proc.getStore()));
	    
     } finally {
       if (weSuspendedThread && (tRef != null)) tRef.resume();
     }
 
   }

  public Object clone() {return new EvaluateExpression();}
  
} // GetLocals

/*
 * $Log: EvaluateExpression.java,v $
 * Revision 1.2  2001/03/24 05:42:36  paulk
 * Updated to reflect reorganization of debugger code.
 *
 * Revision 1.1  2000/08/14 02:37:40  paulk
 * Initial revision.
 *
 * Revision 1.1  2000/04/10 05:33:28  paulk
 * Initial revision.
 *
 *
 */

// End of EvaluateExpression.java
