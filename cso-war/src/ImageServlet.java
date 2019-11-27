import java.awt.image.BufferedImage;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.math.BigInteger;
//import org.apache.commons.codec.binary.Hex;
import java.util.Arrays;

//com.sun.media.jai.codec
import javax.imageio.ImageIO;


import javax.naming.InitialContext;
import javax.resource.ResourceException;
import javax.resource.cci.Connection;
import javax.resource.cci.ConnectionFactory;
import javax.resource.cci.Interaction;
import javax.resource.cci.InteractionSpec;
import javax.resource.cci.Record;
import javax.servlet.RequestDispatcher;
import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.ibm.connector2.cics.ECIInteractionSpec;
import com.ibm.connector2.cics.ECIManagedConnectionFactory;

import org.apache.commons.io.FileUtils;

public class ImageServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	private Connection eciConn;
	private Interaction eciInt;
	private ECIInteractionSpec eSpec;

	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {

		String nationalId = null;
		String formId = null;
		String photo = null;



		try {

			File imageFileJPGImageIO = new File("c:\\tmp\\image-6.jpeg");
			File imageFileJPG		 = new File("c:\\tmp\\image-30.jpg");
			File imageFilePNG		 = new File("c:\\tmp\\image-8.png");
			
			// BufferedImage image = ImageIO.

			byte[] cicsOutArr = execute(nationalId, formId, photo);

			/*
			byte[] outBytePhotoRawImageIO = Arrays.copyOfRange(cicsOutArr, 25, cicsOutArr.length);
			
			ByteArrayInputStream bisRawImageIO = new ByteArrayInputStream(outBytePhotoRawImageIO);
			BufferedImage bImageRawImageIO = ImageIO.read(bisRawImageIO);
			ImageIO.write(bImageRawImageIO, "jpg", imageFileJPGImageIO);
			
			
			*/
			byte[] outBytePhotoRawFileUtils = Arrays.copyOfRange(cicsOutArr, 25, cicsOutArr.length);
			
			
			FileUtils.writeByteArrayToFile(imageFileJPG, outBytePhotoRawFileUtils);
			
			//request.setAttribute("image", imageFileJPG);
			
			/*
			byte[] outBytePhotoRawImageIOPNG = Arrays.copyOfRange(cicsOutArr, 25, cicsOutArr.length);

			
			ByteArrayInputStream bisPNG = new ByteArrayInputStream(outBytePhotoRawImageIOPNG);
			BufferedImage bImagePNG = ImageIO.read(bisPNG);
			ImageIO.write(bImagePNG, "png", imageFilePNG);
			*/
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

		// ByteArrayInputStream bis = new ByteArrayInputStream(outBytePhoto);

		// BufferedImage bImage2 = ImageIO.read(bis);

		// ImageIO.write(bImage2, "jpg", out);
		// out.flush();
		// out.close();

		// FileUtils.writeByteArrayToFile(f, outByteImage);
		/*
		 * File fout = new File(path); BufferedImage image = ImageIO.read(fout);
		 * 
		 * response.setContentType("image/jpeg2000"); ImageIO.write(image, "jpg", out);
		 * out.flush(); out.close();
		 */

	}

	public static String hexToBinary(String hex) {
		int len = hex.length() * 4;
		String bin = new BigInteger(hex, 16).toString(2);

		// left pad the string result with 0s if converting to BigInteger removes them.
		if (bin.length() < len) {
			int diff = len - bin.length();
			String pad = "";
			for (int i = 0; i < diff; ++i) {
				pad = pad.concat("0");
			}
			bin = pad.concat(bin);
		}
		return bin;
	}

	public byte[] executeTransaction(String nationalId, String formId, String photo) throws ResourceException {
		return this.executeServerCall(nationalId, formId, photo);
	}

	public byte[] execute(String nationalId, String formId, String photo) throws ResourceException {
		return this.executeServerCall(nationalId, formId, photo);
	}

	public byte[] executeServerCall(String nationalId, String formId, String photo) throws ResourceException {
		// this.getConnectionLocal();
		this.getConnection();
		JavaStringRecord javaStringRecord = new JavaStringRecord();

		javaStringRecord.setEncoding("IBM420");

		StringBuffer buffer = new StringBuffer();

		//buffer.append("0300020301019921398892208");
		
		buffer.append("0290120816000981245680378");
		//buffer.append("0300020301019921398892208");

		for (int i = 0; i < 20580; i++) {
			buffer.append("0");

		}
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
		return javaStringRecordOut.getBinary();
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