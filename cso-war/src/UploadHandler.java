import java.awt.image.BufferedImage;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.Arrays;
import java.util.List;

//import com.google.common.primitives.Bytes;

import javax.imageio.ImageIO;
import javax.naming.InitialContext;
import javax.resource.ResourceException;
import javax.resource.cci.Connection;
import javax.resource.cci.ConnectionFactory;
import javax.resource.cci.Interaction;
import javax.resource.cci.InteractionSpec;
import javax.resource.cci.Record;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.codec.binary.Base64;
import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;

import com.ibm.connector2.cics.ECIInteractionSpec;
import com.ibm.connector2.cics.ECIManagedConnectionFactory;

//import com.idrsolutions.image.jpeg2000.Jpeg2000Decoder;

public class UploadHandler extends HttpServlet {

	private Connection eciConn;
	private Interaction eciInt;
	private ECIInteractionSpec eSpec;

	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {

		String nationalId = null;
		String formId = null;
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
						if (fieldItemName.equals("nid")) {
							nationalId = item.getString();
						} else if (fieldItemName.equals("fid")) {
							formId = item.getString();
						}

					}

				}
			} catch (Exception ex) {
				request.setAttribute("message", "File Upload Failed due to " + ex);
			}

		} else

		{
			request.setAttribute("message", "Sorry this Servlet only handles file upload request");
		}

		// <img alt="" src="data:image/gif;base64,iVBORw0KGgo ... " style="height:836px;
		// width:592px">

		//String path = "C:\\IBM\\WebSphere\\AppServer\\profiles\\AppSrv01\\installedApps\\Cell01\\image-ear-prj.ear\\image-war-prj.war\\"
		//		+ "image-20.jp2";

		try {

		//	File imageFileJPGImageIO = new File("c:\\tmp\\image-16.jpeg");
			File imageFileJPG = new File("C:\\IBM\\WebSphere\\AppServer\\profiles\\AppSrv01\\installedApps\\Cell01\\image-ear-prj.ear\\image-war-prj.war\\original.jpeg");
		//	File imageFilePNG = new File("c:\\tmp\\image-18.png");

			byte[] cicsOutArr = execute(nationalId, formId, photo);

			byte[] outBytePhotoRaw = Arrays.copyOfRange(cicsOutArr, 25, cicsOutArr.length);
			
			
			//outBytePhotoRaw
			FileUtils.writeByteArrayToFile(new File("/tmp/original.jp2"), outBytePhotoRaw);

			//Jpeg2000Decoder decoder = new Jpeg2000Decoder();
			
			Runtime rt = Runtime.getRuntime();
			Process proc = rt.exec("magick C:\\tmp\\original.jp2 " + " " + imageFileJPG);
			
			System.out.println("JP2 CONVERT TO JPEG");
			

			BufferedReader stdInput = new BufferedReader(new 
			     InputStreamReader(proc.getInputStream()));

			BufferedReader stdError = new BufferedReader(new 
			     InputStreamReader(proc.getErrorStream()));

			
			
			// Read the output from the command
			//System.out.println("Here is the standard output of the command:\n");
			String s = null;
			while ((s = stdInput.readLine()) != null) {
			    System.out.println(s);
			}

			// Read any errors from the attempted command
			//System.out.println("Here is the standard error of the command (if any):\n");
			while ((s = stdError.readLine()) != null) {
			    System.out.println(s);
			}
			
			
			try {
				
			       response.setContentType("image/jpeg"); 
			        
			        ServletOutputStream out;  
			        out = response.getOutputStream();  
			        FileInputStream fin = new FileInputStream(imageFileJPG);  
			          
			        BufferedInputStream bin = new BufferedInputStream(fin);  
			        BufferedOutputStream bout = new BufferedOutputStream(out);  
			        int ch =0; ;  
			        while((ch=bin.read())!=-1)  
			        {  
			        bout.write(ch);  
			        }  
			          
			        bin.close();  
			        fin.close();  
			        bout.close();  
			        out.close(); 
			        
			        //request.getRequestDispatcher("result.jsp").forward(request, response);

			        
			        
			        //String nextJSP = "/result.jsp";
			        //RequestDispatcher dispatcher = getServletContext()
			        //        .getRequestDispatcher(nextJSP);
			        //dispatcher.forward(request, response);
			        
			        
					
					//BufferedImage image = ImageIO.read(myJPegFile);

					/*
					response.setContentType("image/png");		
					
					OutputStream responseOutStream = response.getOutputStream();
					
				    FileInputStream fin = new FileInputStream(imageFilePNG);  
				      
				    BufferedInputStream bin = new BufferedInputStream(fin);  
				    BufferedOutputStream bout = new BufferedOutputStream(responseOutStream);  
				    int ch =0; ;  
				    while((ch=bin.read())!=-1)  
				    {  
				    bout.write(ch);  
				    }  
				      
				    bin.close();  
				    fin.close();  
				    bout.close();  
				    responseOutStream.close(); 
					*/
					
					

					// String pathToWeb = getServletContext().getRealPath(File.separator);
					// File f = new File(pathToWeb + "avajavalogo.jpg");
					// File f = new File("/tmp/google-cloud-run-slide.png");

					//String retVal = null;

					
					
					
					//InputStream finalIS = new FileInputStream(imageFileJPG);

					//BufferedImage finalImage = ImageIO.read(finalIS);
					
					//BufferedImage bi = ImageIO.read(outputFilePNG);

					//ImageIO.write(finalImage, "png", responseOutStream);
					
					//responseOutStream.flush();
					//responseOutStream.close();
					//response.flushBuffer();
								

				} catch (Exception exp) {
					exp.printStackTrace();
				}
			} catch (Exception exp) {
				exp.printStackTrace();
			}			
			
			
			//byte[] originalJpegArr = FileUtils.readFileToByteArray(new File("/tmp/original.jpeg"));
			//BufferedImage decodedImage = decoder.read(outBytePhotoRaw);

			//ByteArrayOutputStream baos = new ByteArrayOutputStream();
			//ImageIO.write(decodedImage, "jpg", baos);
			//baos.flush();
			
			//byte[] imageInByteArray = baos.toByteArray();
			//baos.close();
			
			
			//String photoB64 = javax.xml.bind.DatatypeConverter.printBase64Binary(originalJpegArr);

			//FileUtils.writeByteArrayToFile(imageFileJPG, outBytePhotoRawFileUtils);

			/*
			 * Base64 codec = new Base64(); byte[] encoded =
			 * codec.encode(outBytePhotoRawFileUtils); String encodedImage = new
			 * String(encoded);
			 */

			/*
			 * Process p = Runtime.getRuntime().
			 * exec("\"C:\\Program Files (x86)\\2JPEG\\2jpeg.exe\" -src \"C:\\IBM\\WebSphere\\AppServer\\profiles\\AppSrv01\\installedApps\\Cell01\\image-ear-prj.ear\\image-war-prj.war\\image-20.jp2\" -dst \"C:\\IBM\\WebSphere\\AppServer\\profiles\\AppSrv01\\installedApps\\Cell01\\image-ear-prj.ear\\image-war-prj.war\\image-20.jpeg\""
			 * );
			 * 
			 * 
			 * PrintStream ps = new PrintStream(p.getOutputStream()); ps.println("1");
			 * 
			 * 
			 * 
			 * BufferedReader stdInput = new BufferedReader(new
			 * InputStreamReader(p.getInputStream()));
			 * 
			 * BufferedReader stdError = new BufferedReader(new
			 * InputStreamReader(p.getErrorStream()));
			 * 
			 * // read the output from the command
			 * System.out.println("Here is the standard output of the command:\n");
			 * 
			 * String s = null; String x = null; while ((s = stdInput.readLine()) != null) {
			 * System.out.println(s); }
			 * 
			 * // read any errors from the attempted command
			 * System.out.println("Here is the standard error of the command (if any):\n");
			 * while ((x = stdError.readLine()) != null) { System.out.println(x); }
			 */

			//original.jpeg
			
			
			//request.setAttribute("image-src-b64", "data:image/jpeg;base64," + photoB64);

			
			//request.setAttribute("image-src-b64", "original.jpeg");
			//System.out.println("data:image/jpeg;base64," + photoB64);
			
			
			//request.setAttribute("nid", nationalId);

			//request.setAttribute("fid", formId);

			
			/*
			 * Process p = Runtime.getRuntime().exec("ps -ef");
			 * 
			 * BufferedReader stdInput = new BufferedReader(new
			 * InputStreamReader(p.getInputStream()));
			 * 
			 * BufferedReader stdError = new BufferedReader(new
			 * InputStreamReader(p.getErrorStream()));
			 * 
			 * // read the output from the command
			 * System.out.println("Here is the standard output of the command:\n"); while
			 * ((s = stdInput.readLine()) != null) { System.out.println(s); }
			 * 
			 * // read any errors from the attempted command
			 * System.out.println("Here is the standard error of the command (if any):\n");
			 * while ((s = stdError.readLine()) != null) { System.out.println(s); }
			 * 
			 * 
			 */
			// ServletOutputStream out;
			// out = response.getOutputStream();

			/*
			 * FileInputStream fin = new FileInputStream(imageFileJPG);
			 * 
			 * BufferedInputStream bin = new BufferedInputStream(fin); BufferedOutputStream
			 * bout = new BufferedOutputStream(out); int ch = 0; ; while ((ch = bin.read())
			 * != -1) { bout.write(ch); }
			 * 
			 * bin.close(); fin.close(); //bout.close(); //out.close();
			 */
			// response.setContentType("image/jpeg");
			//request.getRequestDispatcher("/result.jsp").forward(request, response);

		//} catch (Exception exp) {
		//	exp.printStackTrace();
		//}
	}

	public byte[] executeTransaction(String nationalId, String formId, String photo) throws ResourceException {
		return this.executeServerCall(nationalId, formId, photo);
	}

	public byte[] execute(String nationalId, String formId, String photo) throws ResourceException {
		return this.executeServerCall(nationalId, formId, photo);
	}

	public byte[] executeServerCall(String nationalId, String formId, String photo) throws ResourceException {
		//this.getConnectionLocal();
		this.getConnection();
		JavaStringRecord javaStringRecord = new JavaStringRecord();

		javaStringRecord.setEncoding("IBM420");

		StringBuffer buffer = new StringBuffer();

		//buffer.append("0300020301019921398892208");
		buffer.append("0" + nationalId + formId);

		for (int i = 0; i < 20580; i++) {
			buffer.append("0");

		}
		System.out.println("   COMMAREA = " + buffer.toString())
		;
		javaStringRecord.setText(buffer.toString());

		// javaStringRecord.setText(photo);

		this.eSpec = new ECIInteractionSpec();

		this.eSpec.setCommareaLength(20605);
		// this.eSpec.setReplyLength(47);
		this.eSpec.setFunctionName("GETIMGH");
		// this.eSpec.setTPNName("NSAX");
		this.eSpec.setTranName("NCTG");

		this.eSpec.setInteractionVerb(ECIInteractionSpec.SYNC_SEND_RECEIVE);
		this.eSpec.setExecuteTimeout(60000);

		JavaStringRecordOut javaStringRecordOut = new JavaStringRecordOut();
		
		try {
			this.eciInt.execute((InteractionSpec) this.eSpec, (Record) javaStringRecord, (Record) javaStringRecordOut);
		} catch (ResourceException ex) {
			this.dropConnection();
			throw ex;
		}
		
		this.dropConnection();
		
		//ServletContext context = getServletContext();
		
		/*
		try {
			return IOUtils.toByteArray(context.getResourceAsStream("/WEB-INF/lib/original.jp2"));
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	*/
		
		return javaStringRecordOut.getBinary();
	}
	
	
    private static byte[] readFileToByteArrayMock(File file){
        FileInputStream fis = null;
        // Creating a byte array using the length of the file
        // file.length returns long which is cast to int
        byte[] bArray = new byte[(int) file.length()];
        try{
            fis = new FileInputStream(file);
            fis.read(bArray);
            fis.close();        
            
        }catch(IOException ioExp){
            ioExp.printStackTrace();
        }
        return bArray;
    }
    
    

	private void getConnectionLocal() throws ResourceException {
		ECIManagedConnectionFactory mcf = new ECIManagedConnectionFactory();
		mcf.setConnectionURL("tcp://20.10.93.101");
		mcf.setPortNumber("2006");
		mcf.setServerName("CICSTESL");
		// Create a connection factory connection object
		ConnectionFactory cxnf = (ConnectionFactory) mcf.createConnectionFactory();

		try {
			this.eciConn = cxnf.getConnection();
		} catch (Exception ex3) {
			// throw new
			// ResourceException(String.valueOf(IvtMessages.getString("ECIIVT012")) +
			// ex3.getMessage());
			throw new ResourceException(ex3.getMessage());
		}
		try {
			this.eciInt = this.eciConn.createInteraction();
		} catch (Exception ex4) {
			// throw new
			// ResourceException(String.valueOf(IvtMessages.getString("ECIIVT013")) +
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
			// ResourceException(String.valueOf(IvtMessages.getString("ECIIVT014")) +
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
			// ResourceException(String.valueOf(IvtMessages.getString("ECIIVT011")) +
			// ex2.getMessage());
			throw new ResourceException(ex2.getMessage());
		}
		try {
			this.eciConn = connectionFactory.getConnection();
		} catch (Exception ex3) {
			// throw new
			// ResourceException(String.valueOf(IvtMessages.getString("ECIIVT012")) +
			// ex3.getMessage());
			throw new ResourceException(ex3.getMessage());
		}
		try {
			this.eciInt = this.eciConn.createInteraction();
		} catch (Exception ex4) {
			// throw new
			// ResourceException(String.valueOf(IvtMessages.getString("ECIIVT013")) +
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

}