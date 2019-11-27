import java.io.OutputStream;
import java.io.InputStream;
import javax.resource.cci.Record;
import javax.resource.cci.Streamable;

public class JavaStringRecord implements Streamable, Record
{
    //static final String copyright_notice = "Licensed Materials - Property of IBM 5724-I81,5655-Y20 (c) Copyright IBM Corp. 2009, 2012 All Rights Reserved. US Government Users Restricted Rights - Use, duplication or disclosure restricted by GSA ADP Schedule Contract with IBM Corp.";
    //public static final String CLASS_VERSION = "@(#) java/com/ibm/ctg/ivt/jee/JavaStringRecord.java, cd_gw_API_J2EE, c920-bsf c920-20160304-1318";
    static final long serialVersionUID = 1L;
    private String recordName;
    private String desc;
    private String contents;
    private String enc;
    
    public JavaStringRecord() {
        this.contents = new String("");
        this.enc = "ASCII";
    }
    
    public String getRecordName() {
        return this.recordName;
    }
    
    public void setRecordName(final String recordName) {
        this.recordName = recordName;
    }
    
    public void setRecordShortDescription(final String desc) {
        this.desc = desc;
    }
    
    public String getRecordShortDescription() {
        return this.desc;
    }
    
    @Override
    public int hashCode() {
        if (this.contents != null) {
            return this.contents.hashCode();
        }
        return super.hashCode();
    }
    
    @Override
    public boolean equals(final Object o) {
        return o instanceof JavaStringRecord && (o == this || ((JavaStringRecord)o).getText().equals(this.getText()));
    }
    
    public Object clone() throws CloneNotSupportedException {
        return super.clone();
    }
    
    public void read(final InputStream inputStream) {
        try {
            final int available = inputStream.available();
            byte[] array = null;
            if (available > 0) {
                array = new byte[available];
                inputStream.read(array);
            }
            this.contents = new String(array, this.enc);
        }
        catch (Exception ex) {
            ex.printStackTrace();
        }
    }
    
    public void write(final OutputStream outputStream) {
        try {
            outputStream.write(this.contents.getBytes(this.enc));
        }
        catch (Exception ex) {
            ex.printStackTrace();
        }
    }
    
    public String getText() {
        return this.contents;
    }
    
    public void setText(final String contents) {
        this.contents = contents;
    }
    
    public String getEncoding() {
        return this.enc;
    }
    
    public void setEncoding(final String enc) {
        this.enc = enc;
    }
}
