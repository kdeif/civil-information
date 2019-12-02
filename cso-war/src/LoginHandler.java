import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.util.List;

import javax.naming.InitialContext;
import javax.resource.ResourceException;
import javax.resource.cci.Connection;
import javax.resource.cci.ConnectionFactory;
import javax.resource.cci.Interaction;
import javax.resource.cci.InteractionSpec;
import javax.resource.cci.Record;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;

import com.ibm.connector2.cics.ECIInteractionSpec;
import com.ibm.connector2.cics.ECIManagedConnectionFactory;
import com.ibm.ctg.client.EPIRequest;
import com.ibm.ctg.client.ESIRequest;
import com.ibm.ctg.client.JavaGateway;
import com.ibm.ctg.epi.AID;
import com.ibm.ctg.epi.EPIException;
import com.ibm.ctg.epi.EPIGateway;
import com.ibm.ctg.epi.EPITerminal;
import com.ibm.ctg.epi.Field;
import com.ibm.ctg.epi.Screen;
import com.ibm.ctg.epi.Terminal;
import com.ibm.ctg.epi.Terminal.EPISignOnType;
import com.ibm.ctg.epi.TerminalException;

import javax.naming.InitialContext;
import javax.resource.ResourceException;
import javax.resource.cci.Connection;
import javax.resource.cci.ConnectionFactory;
import javax.resource.cci.Interaction;
import javax.resource.cci.InteractionSpec;
import javax.resource.cci.Record;

public class LoginHandler extends HttpServlet {

	private final String UPLOAD_DIRECTORY = "/tmp";

	private Connection eciConn;
	private Interaction eciInt;
	private ECIInteractionSpec eSpec;

	// private EPIInteractionSpec eSpec;

	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {

		String userId = null;
		String password = null;
		String photo = null;

		// process only if its multipart content
		if (ServletFileUpload.isMultipartContent(request)) {
			try {

				List<FileItem> multiparts = new ServletFileUpload(new DiskFileItemFactory()).parseRequest(request);

				for (FileItem item : multiparts) {

					if (item.getFieldName().equals("photo")) {
						photo = item.getString();
					} else {
						String fieldItemName = item.getFieldName();
						if (fieldItemName.equals("userId")) {
							userId = item.getString();
						} else if (fieldItemName.equals("password")) {
							password = item.getString();
						}

					}

				}

				byte[] cicsOutArr = execute(userId, password);

			} catch (Exception ex) {
				request.setAttribute("message", "File Upload Failed due to " + ex);
			}

		} else

		{
			request.setAttribute("message", "Sorry this Servlet only handles file upload request");
		}
	}

	public byte[] executeTransaction(String userId, String password) throws ResourceException {
		return this.executeServerCall(userId, password);
	}

	public byte[] execute(String userId, String password) throws ResourceException {
		return this.executeServerCall(userId, password);
	}

	/*
	 * public byte[] executeTransaction(String userId, String password) throws
	 * ResourceException { return this.executeServerCall(userId, password); }
	 * 
	 * public byte[] execute(String userId, String password) throws
	 * ResourceException { return this.executeServerCall(userId, password); }
	 */
	public byte[] executeServerCall(String userId, String password) throws ResourceException {
		// this.getConnectionLocal();
		// this.getConnection();

		String test = null;
		
		JavaGateway javaGateway =null;
			try {
				 javaGateway = new JavaGateway("tcp://20.10.93.101", 2006);
				
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}

			// EPIRequest myEPIRequest = EPIRequest.addTerminal("CICSTESL",null,
			// null);

			// javaGateway.
			/*
			 * byte[] myByteArrayData = new byte[3270];
			 * myEPIRequest.startTran("NSAX", myByteArrayData, 10);
			 * 
			 * javaGateway.flow(myEPIRequest);
			 * 
			 * System.out.println("OUTPUT = " + myEPIRequest.getCicsRc() + " " +
			 * myEPIRequest.getCicsRcString() + " "+
			 * myEPIRequest.getClientType());
			 * 
			 * 
			 */
			//EPIGateway epiGate = new EPIGateway("tcp://20.10.93.101", 2006);
			
			//Terminal term = new Terminal(epiGate,"CICSTESL", "IO1V3CVV","3278", Terminal.EPI_SIGNON_INCAPABLE, "���� ��� ���","120875",0, "Cp420" );
			
			//term.setExtendedTerminal(true);
			//term.connect();
			
			//epiGate.serverCount()
			
			//epiGate.open();
		

			
				//Terminal term = new Terminal();
				//term.setGateway(epiGate);
				
				ESIRequest ESIReq = new ESIRequest();
			
				String userIdCorrectCP = null;
				
					userIdCorrectCP = new String ("NHAZEM");
				
				
				ESIReq.setUserid(userIdCorrectCP);
				
				ESIReq.setCurrentPassword("NOURTYUI");
				ESIReq.setServer("CICSTESL");
				
				
				//ESIReq.
				
				
				try {
					javaGateway.flow(ESIReq);
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				System.out.println(ESIReq.getGatewayRcString());
				System.out.println(ESIReq.getCicsRcString());
				
				
				//System.out.println(epiGate.serverCount());
				
				/*term.setSignonCapability(Terminal.EPI_SIGNON_INCAPABLE);
				term.setExtendedTerminal(true);
				term.setEncoding("Cp420");
				term.setUserid("���� ��� ���");
				term.setPassword("120875");
				term.connect();

				Screen scr = term.getScreen();
				Field fld = scr.field(1);
				fld.setText("NSAX");
				scr.setAID(AID.enter);
				
				term.disconnect();
				epiGate.close();
*/
			

			// term.setSignonCapability(Terminal.EPI_SIGNON_INCAPABLE);
			// term.setExtendedTerminal(true);
			// term.setUserid("test");
			// term.setPassword("test");

			/*
			 * 
			 * EPITerminal term = new EPITerminal();
			 * 
			 * 
			 * 
			 * term.setGatewayURL("20.10.93.101"); term.setATI(true);
			 * term.setUserid("���� ��� ���"); term.setPassword("120875");
			 * term.setTransaction("NSAX");
			 * 
			 * term.startTran(); term.connect();
			 * 
			 * System.out.println(term.getState());
			 * 
			 * Screen scr = term.getScreen(); Field fld = scr.field(1);
			 * fld.setText("NSA01"); scr.setAID(AID.enter); term.send(); for
			 * (int i = 1; i <= scr.fieldCount(); i++) { fld = scr.field(i); //
			 * get field by index if (fld.textLength() > 0) {
			 * 
			 * test = fld.getText(); System.out.println("Field " + i + ":" +
			 * test);
			 * 
			 * }
			 * 
			 * } term.disconnect();
			 * 
			 */

			// EPIRequest epiRequest = new EPIRequest();
			/*
			 * Terminal term = new Terminal(epiGate, "CICSTESL", "I01v3cvv",
			 * "3278",Terminal.EPI_SIGNON_INCAPABLE,"test","test", 6000,
			 * "Cp420");
			 * 
			 */

			// epiGate.close();

			/*
			 * } catch (EPIException epiEx) { epiEx.printStackTrace();
			 */
		

		return test.getBytes();
		/*
		 * JavaStringRecord javaStringRecord = new JavaStringRecord();
		 * 
		 * javaStringRecord.setEncoding("IBM420");
		 * 
		 * String filename = "/WEB-INF/commarea.in";
		 * 
		 * ServletContext context = getServletContext();
		 * 
		 * StringBuffer buffer = new StringBuffer();
		 * 
		 * // First get the file InputStream using
		 * ServletContext.getResourceAsStream() // method. InputStream is =
		 * context.getResourceAsStream(filename); if (is != null) {
		 * InputStreamReader isr = new InputStreamReader(is); BufferedReader
		 * reader = new BufferedReader(isr);
		 * 
		 * String text;
		 * 
		 * // We read the file line by line and later will be displayed on the
		 * // browser page. try { while ((text = reader.readLine()) != null) {
		 * buffer.append(text); } } catch (IOException e) { // TODO
		 * Auto-generated catch block e.printStackTrace(); } }
		 * 
		 * for (int i = 0; i < 32724; i++) { buffer.append("0");
		 * 
		 * }
		 * 
		 * // buffer.append("0300020301019921398892208")
		 * 
		 * // buffer.append(userId+password);
		 * 
		 * // buffer.append(userId + password);
		 * 
		 * /* for (int i = 0; i < 20580; i++) { buffer.append("0");
		 * 
		 * }
		 */
		/*
		 * System.out.println(" LOGIN  COMMAREA = " + buffer.toString());
		 * javaStringRecord.setText(buffer.toString());
		 * 
		 * // javaStringRecord.setText(photo);
		 * 
		 * this.eSpec = new ECIInteractionSpec();
		 * 
		 * this.eSpec.setCommareaLength(32763); //
		 * this.eSpec.setCommareaLength(54); // this.eSpec.setReplyLength(47);
		 * this.eSpec.setFunctionName("LGONCHK"); //
		 * this.eSpec.setTPNName("NSAX"); this.eSpec.setTranName("NSAX");
		 * 
		 * this.eSpec.setInteractionVerb(ECIInteractionSpec.SYNC_SEND_RECEIVE);
		 * this.eSpec.setExecuteTimeout(60000);
		 * 
		 * JavaStringRecordOut javaStringRecordOut = new JavaStringRecordOut();
		 * 
		 * try { this.eciInt.execute((InteractionSpec) this.eSpec, (Record)
		 * javaStringRecord, (Record) javaStringRecordOut); } catch
		 * (ResourceException ex) { this.dropConnection(); throw ex; }
		 * 
		 * this.dropConnection();
		 * 
		 * // ServletContext context = getServletContext();
		 * 
		 * /* try { return IOUtils.toByteArray(context.getResourceAsStream(
		 * "/WEB-INF/lib/original.jp2")) ; } catch (IOException e) { // TODO
		 * Auto-generated catch block e.printStackTrace(); }
		 */

		/*
		 * return javaStringRecordOut.getBinary();
		 * 
		 */
	}

	private static byte[] readFileToByteArrayMock(File file) {
		FileInputStream fis = null;
		// Creating a byte array using the length of the file
		// file.length returns long which is cast to int
		byte[] bArray = new byte[(int) file.length()];
		try {
			fis = new FileInputStream(file);
			fis.read(bArray);
			fis.close();

		} catch (IOException ioExp) {
			ioExp.printStackTrace();
		}
		return bArray;
	}

	private void getConnectionLocal() throws ResourceException {
		ECIManagedConnectionFactory mcf = new ECIManagedConnectionFactory();
		mcf.setConnectionURL("tcp:// 20.10.93.101");
		mcf.setPortNumber("2006");
		mcf.setServerName("CICSTESL");
		// Create a connection factory connection object
		ConnectionFactory cxnf = (ConnectionFactory) mcf.createConnectionFactory();

		try {
			this.eciConn = cxnf.getConnection();
		} catch (Exception ex3) {
			// throw new
			// ResourceException(String.valueOf(IvtMessages.getString("ECIIVT012"))
			// +
			// ex3.getMessage());
			throw new ResourceException(ex3.getMessage());
		}
		try {
			this.eciInt = this.eciConn.createInteraction();
		} catch (Exception ex4) {
			// throw new
			// ResourceException(String.valueOf(IvtMessages.getString("ECIIVT013"))
			// +
			// ex4.getMessage())
			throw new ResourceException(ex4.getMessage());
		}
	}

	private void getConnection() throws ResourceException {
		Object lookup;
		try {
			lookup = new InitialContext().lookup("eis/CICSTG");
			// lookup = new InitialContext().lookup("java:comp/env/ECI");
		} catch (Exception ex) {
			// throw new
			// ResourceException(String.valueOf(IvtMessages.getString("ECIIVT014"))
			// +
			// ex.getMessage());
			throw new ResourceException(ex.getMessage());

		}
		if (lookup == null) {
			// throw new ResourceException(IvtMessages.getString("ECIIVT010"));
			throw new ResourceException("lookup error");
		}
		ConnectionFactory connectionFactory;
		try {
			connectionFactory = (ConnectionFactory) lookup;
		} catch (ClassCastException ex2) {
			// throw new
			// ResourceException(String.valueOf(IvtMessages.getString("ECIIVT011"))
			// +
			// ex2.getMessage());
			throw new ResourceException(ex2.getMessage());
		}
		try {
			this.eciConn = connectionFactory.getConnection();
		} catch (Exception ex3) {
			// throw new
			// ResourceException(String.valueOf(IvtMessages.getString("ECIIVT012"))
			// +
			// ex3.getMessage());
			throw new ResourceException(ex3.getMessage());
		}
		try {
			this.eciInt = this.eciConn.createInteraction();
		} catch (Exception ex4) {
			// throw new
			// ResourceException(String.valueOf(IvtMessages.getString("ECIIVT013"))
			// +
			// ex4.getMessage())
			throw new ResourceException(ex4.getMessage());
		}
	}

	private void dropConnection() {
		try {
			this.eciInt.close();
			this.eciConn.close();
		} catch (Exception ex) {
			ex.printStackTrace();
		}
		this.eciInt = null;
		this.eciConn = null;
	}

	/*
	 * String userId = null; String password = null;
	 * 
	 * 
	 * // process only if its multipart content if
	 * (ServletFileUpload.isMultipartContent(request)) { try {
	 * 
	 * List<FileItem> multiparts = new ServletFileUpload(new
	 * DiskFileItemFactory()).parseRequest(request);
	 * 
	 * for (FileItem item : multiparts) { String fieldItemName =
	 * item.getFieldName(); if (fieldItemName.equals("userid")) { userId =
	 * item.getString(); } else if (fieldItemName.equals("password")) { password
	 * = item.getString(); }
	 * 
	 * }
	 * 
	 * 
	 * 
	 * // String name = new File(item.getName()).getName(); // item.write( new
	 * File(UPLOAD_DIRECTORY + File.separator + name));
	 * 
	 * // item. //execute();
	 * 
	 * // File uploaded successfully
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * request.getRequestDispatcher("/result.jsp").forward(request, response);
	 * 
	 * }
	 * 
	 * public String executeTransaction(String nationalId, String formId, String
	 * photo) throws ResourceException { // return
	 * this.executeServerCall(nationalId, formId, photo); }
	 * 
	 * public String execute(String userId, String password) throws
	 * ResourceException { return this.executeServerCall(userId, password); }
	 * 
	 * public String executeServerCall(String userId, String password) throws
	 * ResourceException { this.getConnectionLocal(userId, password);
	 * 
	 * this.dropConnection(); return javaStringRecord.getText(); }
	 * 
	 * private void getConnectionLocal(String userId, String password) throws
	 * ResourceException {
	 * 
	 * EPIGateway epiGateway = new EPIGateway("20.10.93.101", 2006);
	 * 
	 * Terminal terminal = new Terminal(epiGateway, "CICSTESL", null, null);
	 * 
	 * terminal.send("NSAX", "0" + userId + password);
	 * 
	 * ECIManagedConnectionFactory mcf = new ECIManagedConnectionFactory();
	 * mcf.setConnectionURL("tcp://20.10.93.101"); mcf.setPortNumber("2006");
	 * mcf.setServerName("CICSTESL"); // Create a connection factory connection
	 * object ConnectionFactory cxnf = (ConnectionFactory)
	 * mcf.createConnectionFactory();
	 * 
	 * try { this.eciConn = cxnf.getConnection(); } catch (Exception ex3) { //
	 * throw new //
	 * ResourceException(String.valueOf(IvtMessages.getString("ECIIVT012")) + //
	 * ex3.getMessage()); throw new ResourceException(ex3.getMessage()); } try {
	 * this.eciInt = this.eciConn.createInteraction(); } catch (Exception ex4) {
	 * // throw new //
	 * ResourceException(String.valueOf(IvtMessages.getString("ECIIVT013")) + //
	 * ex4.getMessage()) throw new ResourceException(ex4.getMessage()); } }
	 * 
	 * private void getConnection() throws ResourceException { Object lookup;
	 * try { lookup = new InitialContext().lookup("eis/CICSTG"); // lookup = new
	 * InitialContext().lookup("java:comp/env/ECI"); } catch (Exception ex) { //
	 * throw new //
	 * ResourceException(String.valueOf(IvtMessages.getString("ECIIVT014")) + //
	 * ex.getMessage()); throw new ResourceException(ex.getMessage());
	 * 
	 * } if (lookup == null) { // throw new
	 * ResourceException(IvtMessages.getString("ECIIVT010")); throw new
	 * ResourceException("lookup error"); } ConnectionFactory connectionFactory;
	 * try { connectionFactory = (ConnectionFactory) lookup; } catch
	 * (ClassCastException ex2) { // throw new //
	 * ResourceException(String.valueOf(IvtMessages.getString("ECIIVT011")) + //
	 * ex2.getMessage()); throw new ResourceException(ex2.getMessage()); } try {
	 * this.eciConn = connectionFactory.getConnection(); } catch (Exception ex3)
	 * { // throw new //
	 * ResourceException(String.valueOf(IvtMessages.getString("ECIIVT012")) + //
	 * ex3.getMessage()); throw new ResourceException(ex3.getMessage()); } try {
	 * this.eciInt = this.eciConn.createInteraction(); } catch (Exception ex4) {
	 * // throw new //
	 * ResourceException(String.valueOf(IvtMessages.getString("ECIIVT013")) + //
	 * ex4.getMessage()) throw new ResourceException(ex4.getMessage()); } }
	 * 
	 * private void dropConnection() { try { this.eciInt.close();
	 * this.eciConn.close(); } catch (Exception ex) { ex.printStackTrace(); }
	 * this.eciInt = null; this.eciConn = null; }
	 */

}