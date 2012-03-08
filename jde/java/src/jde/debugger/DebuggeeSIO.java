package jde.debugger;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.BufferedReader;
import java.net.Socket;
import java.io.BufferedWriter;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.ServerSocket;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.InterruptedIOException;

/**
 * DebuggeeSIO.java
 *
 *
 * Created: Sun Feb 18 01:24:09 2001
 *
 * @author <a href="mailto: "</a>
 * @version
 */

public class DebuggeeSIO implements Protocol {

  public DebuggeeSIO (DebuggeeProcess proc){
    this.proc = proc;
    procID = proc.getId();
  }

  /**
   * Launches a thread to connect the Emacs standard I/O buffer
   * for the current process to the standard I/O of the process.
   *
   * <p>
   * This method creates a socket for the standard I/O connection.
   * The thread waits for Emacs to connect to the standard I/O socket.
   *
   * @return Address of standard I/O socket.
   * @exception JDEException if an error occurs
   */
  public int initConnect(final Integer cmdId) throws JDEException {

    jde.signal(procID, MESSAGE, "initSIOConnect: starting standard I/O handshake.");

    ServerSocket ss = null;
    try {
      ss = new ServerSocket(0);
    } catch (IOException ex) {
      throw new JDEException("Unable to create a server socket");
    }

    final ServerSocket sstmp = ss;
    final int port = ss.getLocalPort();

    standardIOConnectThread = new Thread("Standard I/O Thread for App #"+procID) {
	public void run() {
	  try {
	    sstmp.setSoTimeout(15000);

            //   Note!!!   Added to solve initial hang up problem
	    jde.signalCommandResult(procID, cmdId, new Integer(port));

	    jde.signal(procID, MESSAGE, "Debugger waiting for Emacs to connect to app SIO port " +
		   port + ".");

	    sioSocket = sstmp.accept();
	    sstmp.close();
	    initTransport();
	  } catch (IOException ex) {
	    jde.signal(procID, ERROR, "Gave up waiting for Emacs to connect to SIO port: " + port);
	    // proc.shutdown();
	  } catch (SecurityException ex1) {
	    jde.signal(procID, ERROR, "Security exception occurred while connecting to app SIO port " +
		   port);
	  }
	}
      };

    jde.signal(procID, MESSAGE, "initSIOConnect: starting SIO connect thread.");
    standardIOConnectThread.start();
    return port;

  }

  /**
   * Describe <code>initTransport</code> method here.
   *
   */
  public void initTransport() {

      jde.signal(procID, MESSAGE, "Debugger connected to standard I/O socket.");


      final Process process = proc.getVM().process();
      standardInputProcessor = new StandardInputProcessor(process.getOutputStream());
      standardInputProcessor.start();

      standardOutputWriter = new StandardOutputWriter(sioSocket);
      standardOutputWriter.println("*** Process Standard I/O ***");

      standardOutputProcessor = new StandardOutputProcessor(process.getInputStream());
      standardOutputProcessor.start();

      standardErrorProcessor = new StandardErrorProcessor(process.getErrorStream());
      standardErrorProcessor.start();

  }

  public void shutdown() {

    try {
      sioSocket.close();
    } catch (IOException e) {
      
    } // end of try-catch
    
  }

  /**
   * Reads standard input from Emacs and forwards it to the application.
   *
   * @author Paul Kinnucan <paulk@mathworks.com>
   * @version 1.0
   * @since 1.0
   * @see Thread
   */
  private class StandardInputProcessor extends Thread {

    public StandardInputProcessor(final OutputStream toVmStream) {
      super("Input Processor for App #"+procID);
      
      toVM = new PrintStream(toVmStream, true);

      try {
	fromEmacs =
	  new BufferedReader(new InputStreamReader(sioSocket.getInputStream()));
      }
      catch (IOException ex1) {
	jde.signal(procID, ERROR, "Could not get standard input stream from Emacs.");
      }
	
      // setPriority(Thread.MAX_PRIORITY-1);

    }

    public void run() {

      if (fromEmacs == null) return;
      
      try {
	String line;
	while ((line = fromEmacs.readLine()) != null) {
	  toVM.println(line);
	  toVM.flush();
	}

	if (!proc.isShuttingDown()) {
	  try {
	    // sioSocket.close();
	    jde.signal(procID, MESSAGE, "Process closed its standard input.");
	  } catch (Exception ex) {
	    jde.signal(procID, MESSAGE, "Couldn't close socket to standard input.");
	  }
	}
			
      } catch (IOException ex) {
	if (!proc.isShuttingDown()) {
	  try {
	    // sioSocket.close();
	    jde.signal(procID, ERROR, "Input error; application I/O closed");
	  } catch (Exception e) {
	    jde.signal(procID, ERROR, "Input error; couldn't close application I/O");
	  }
	}
      }
    }

    PrintStream toVM;
    BufferedReader fromEmacs;

  }


  /**
   * Writes a line to the socket connected to the
   * standard I/O buffer maintained by Emacs for this
   * application.
   *
   * <p>This class is used by the StandardOutputProcessor
   * and StandardErrorProcessor to forward the application's
   * standard ouput and error output to Emacs.
   *
   * @author "" <paulk@mathworks.com>
   * @version 1.0
   * @since 1.0
   */
  private class StandardOutputWriter {

    public StandardOutputWriter(Socket sioSocket) {
      if (sioSocket == null) {
	jde.signal(procID, ERROR, "Could not transport app output. " +
	       "Transport socket does not exist.");
	return;   
      }

      OutputStream toEmacsStream;

      try {
	toEmacsStream = sioSocket.getOutputStream();
	if (toEmacsStream == null) {
	  jde.signal(procID, ERROR, "Could not transport app output. Transport socket closed.");
	  return;
	}
      }
      catch (IOException ex1) {
	jde.signal(procID, ERROR, "Could not transport app output. Transport socket closed.");
	return;
      }

      toEmacs = new BufferedWriter(new OutputStreamWriter(toEmacsStream));
    }
    
    public void write(char[] cbuf, int len) {
      if (toEmacs != null) {
	try {
	  toEmacs.write(cbuf, 0, len);
	  toEmacs.flush();
	}
	catch (IOException ex1) {
	  jde.signal(procID, ERROR, "I/O error: cannot write process output to Emacs.");
	}
      }
    }

    public void println(String line) {
      try {
	toEmacs.write(line);
	toEmacs.newLine();
      } catch (IOException e) {
	  jde.signal(procID, ERROR, "I/O error: cannot write process output to Emacs.");	
      } // end of try-catch
      
    }

    BufferedWriter toEmacs;
  }


  /**
   * Forwards the application's standard output to Emacs.
   *
   * @author Paul Kinnucan <paulk@mathworks.com>
   * @version 1.0
   * @since 1.0
   * @see Thread
   */
  private class StandardOutputProcessor extends Thread {

   public StandardOutputProcessor(InputStream fromVMStream) {
      fromVM = new BufferedReader(new InputStreamReader(fromVMStream));
      setPriority(Thread.MAX_PRIORITY-1);
    }

    public void run() {

      String line;

      try {
	char[] cbuf = new char[256];
	int len;
	while ((len = fromVM.read(cbuf, 0, 256)) != -1) {
	  synchronized (standardOutputWriter) {
	    if (standardOutputWriter != null) {
	      standardOutputWriter.write(cbuf, len);      
	    } // end of if ()    
	  }
	}
      }
      catch (IOException ex) {
      }

      if (!proc.isShuttingDown()) {
	try {
	  // sioSocket.close();
	  jde.signal(procID, MESSAGE, "Closed transport for application's standard output.");
	} catch (Exception ex) {
	  jde.signal(procID, ERROR, "Could not close application standard output transport.");
	}
      }
    }

    BufferedReader fromVM;

  }


  /**
   * Forwards the application's error output to Emacs.
   *
   * @author Paulk Kinnucan <paulk@mathworks.com>
   * @version 1.0
   * @since 1.0
   * @see Thread
   */
  private class StandardErrorProcessor extends Thread {

    public StandardErrorProcessor(InputStream fromVMStream) {
      super("Standard Error Processor for App #" + procID);
      fromVM = new BufferedReader(new InputStreamReader(fromVMStream));
      setPriority(Thread.MAX_PRIORITY-1);
    }

    public void run() {

      String line;

      try {
	char[] cbuf = new char[256];
	int len;
	while ((len = fromVM.read(cbuf, 0, 256)) != -1) {
	  synchronized (standardOutputWriter) {
	    if (standardOutputWriter != null) {
	      standardOutputWriter.write(cbuf, len);      
	    } // end of if ()      
	  }
	}
      }
      catch (IOException ex) {
      }

      if (!proc.isShuttingDown()) {
	try {
	  // sioSocket.close();
	  jde.signal(procID, MESSAGE, "Closed transport for application's standard error output.");
	} catch (Exception ex) {
	  jde.signal(procID, ERROR, "Could not close application standard error output transport.");
	}
      }
    }

    BufferedReader fromVM;

  }

  DebuggeeProcess proc;

  Integer procID;

  JDE jde = JDE.getJDE();

  /** Socket connection to do i/o */
  Socket sioSocket = null;


  Thread standardIOConnectThread;
  StandardInputProcessor standardInputProcessor;
  StandardOutputProcessor standardOutputProcessor;
  StandardErrorProcessor standardErrorProcessor;
  StandardOutputWriter standardOutputWriter;



}// DebuggeeSIO
