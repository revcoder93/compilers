import java.util.*;
import java.io.*;
/* semantic value of token returned by scanner */
public class lexer
{
	public static void main (String[] args) throws IOException
	{
		BufferedReader reader = new BufferedReader(new FileReader("input.hs"));
		BufferedWriter writer = new BufferedWriter(new FileWriter("output.txt"));
		Yylex yy = new Yylex (reader);
		while(true)
		{
			String x =yy.next_token();
			if(x==null)
				break;
			writer.write(x);	
			writer.write('\n');
		}
		reader.close();
		writer.close();
	}
}


class Yylex {
	private final int YY_BUFFER_SIZE = 512;
	private final int YY_F = -1;
	private final int YY_NO_STATE = -1;
	private final int YY_NOT_ACCEPT = 0;
	private final int YY_START = 1;
	private final int YY_END = 2;
	private final int YY_NO_ANCHOR = 4;
	private final int YY_BOL = 128;
	private final int YY_EOF = 129;

  public String sourceFilename;
	private java.io.BufferedReader yy_reader;
	private int yy_buffer_index;
	private int yy_buffer_read;
	private int yy_buffer_start;
	private int yy_buffer_end;
	private char yy_buffer[];
	private boolean yy_at_bol;
	private int yy_lexical_state;

	Yylex (java.io.Reader reader) {
		this ();
		if (null == reader) {
			throw (new Error("Error: Bad input stream initializer."));
		}
		yy_reader = new java.io.BufferedReader(reader);
	}

	Yylex (java.io.InputStream instream) {
		this ();
		if (null == instream) {
			throw (new Error("Error: Bad input stream initializer."));
		}
		yy_reader = new java.io.BufferedReader(new java.io.InputStreamReader(instream));
	}

	private Yylex () {
		yy_buffer = new char[YY_BUFFER_SIZE];
		yy_buffer_read = 0;
		yy_buffer_index = 0;
		yy_buffer_start = 0;
		yy_buffer_end = 0;
		yy_at_bol = true;
		yy_lexical_state = YYINITIAL;
	}

	private boolean yy_eof_done = false;
	private final int BCOMMENT = 2;
	private final int YYINITIAL = 0;
	private final int COMMENT = 1;
	private final int yy_state_dtrans[] = {
		0,
		39,
		42
	};
	private void yybegin (int state) {
		yy_lexical_state = state;
	}
	private int yy_advance ()
		throws java.io.IOException {
		int next_read;
		int i;
		int j;

		if (yy_buffer_index < yy_buffer_read) {
			return yy_buffer[yy_buffer_index++];
		}

		if (0 != yy_buffer_start) {
			i = yy_buffer_start;
			j = 0;
			while (i < yy_buffer_read) {
				yy_buffer[j] = yy_buffer[i];
				++i;
				++j;
			}
			yy_buffer_end = yy_buffer_end - yy_buffer_start;
			yy_buffer_start = 0;
			yy_buffer_read = j;
			yy_buffer_index = j;
			next_read = yy_reader.read(yy_buffer,
					yy_buffer_read,
					yy_buffer.length - yy_buffer_read);
			if (-1 == next_read) {
				return YY_EOF;
			}
			yy_buffer_read = yy_buffer_read + next_read;
		}

		while (yy_buffer_index >= yy_buffer_read) {
			if (yy_buffer_index >= yy_buffer.length) {
				yy_buffer = yy_double(yy_buffer);
			}
			next_read = yy_reader.read(yy_buffer,
					yy_buffer_read,
					yy_buffer.length - yy_buffer_read);
			if (-1 == next_read) {
				return YY_EOF;
			}
			yy_buffer_read = yy_buffer_read + next_read;
		}
		return yy_buffer[yy_buffer_index++];
	}
	private void yy_move_end () {
		if (yy_buffer_end > yy_buffer_start &&
		    '\n' == yy_buffer[yy_buffer_end-1])
			yy_buffer_end--;
		if (yy_buffer_end > yy_buffer_start &&
		    '\r' == yy_buffer[yy_buffer_end-1])
			yy_buffer_end--;
	}
	private boolean yy_last_was_cr=false;
	private void yy_mark_start () {
		yy_buffer_start = yy_buffer_index;
	}
	private void yy_mark_end () {
		yy_buffer_end = yy_buffer_index;
	}
	private void yy_to_mark () {
		yy_buffer_index = yy_buffer_end;
		yy_at_bol = (yy_buffer_end > yy_buffer_start) &&
		            ('\r' == yy_buffer[yy_buffer_end-1] ||
		             '\n' == yy_buffer[yy_buffer_end-1] ||
		             2028/*LS*/ == yy_buffer[yy_buffer_end-1] ||
		             2029/*PS*/ == yy_buffer[yy_buffer_end-1]);
	}
	private java.lang.String yytext () {
		return (new java.lang.String(yy_buffer,
			yy_buffer_start,
			yy_buffer_end - yy_buffer_start));
	}
	private int yylength () {
		return yy_buffer_end - yy_buffer_start;
	}
	private char[] yy_double (char buf[]) {
		int i;
		char newbuf[];
		newbuf = new char[2*buf.length];
		for (i = 0; i < buf.length; ++i) {
			newbuf[i] = buf[i];
		}
		return newbuf;
	}
	private final int YY_E_INTERNAL = 0;
	private final int YY_E_MATCH = 1;
	private java.lang.String yy_error_string[] = {
		"Error: Internal error.\n",
		"Error: Unmatched input.\n"
	};
	private void yy_error (int code,boolean fatal) {
		java.lang.System.out.print(yy_error_string[code]);
		java.lang.System.out.flush();
		if (fatal) {
			throw new Error("Fatal Error.\n");
		}
	}
	private int[][] unpackFromString(int size1, int size2, String st) {
		int colonIndex = -1;
		String lengthString;
		int sequenceLength = 0;
		int sequenceInteger = 0;

		int commaIndex;
		String workString;

		int res[][] = new int[size1][size2];
		for (int i= 0; i < size1; i++) {
			for (int j= 0; j < size2; j++) {
				if (sequenceLength != 0) {
					res[i][j] = sequenceInteger;
					sequenceLength--;
					continue;
				}
				commaIndex = st.indexOf(',');
				workString = (commaIndex==-1) ? st :
					st.substring(0, commaIndex);
				st = st.substring(commaIndex+1);
				colonIndex = workString.indexOf(':');
				if (colonIndex == -1) {
					res[i][j]=Integer.parseInt(workString);
					continue;
				}
				lengthString =
					workString.substring(colonIndex+1);
				sequenceLength=Integer.parseInt(lengthString);
				workString=workString.substring(0,colonIndex);
				sequenceInteger=Integer.parseInt(workString);
				res[i][j] = sequenceInteger;
				sequenceLength--;
			}
		}
		return res;
	}
	private int yy_acpt[] = {
		/* 0 */ YY_NOT_ACCEPT,
		/* 1 */ YY_NO_ANCHOR,
		/* 2 */ YY_NO_ANCHOR,
		/* 3 */ YY_NO_ANCHOR,
		/* 4 */ YY_NO_ANCHOR,
		/* 5 */ YY_NO_ANCHOR,
		/* 6 */ YY_NO_ANCHOR,
		/* 7 */ YY_NO_ANCHOR,
		/* 8 */ YY_NO_ANCHOR,
		/* 9 */ YY_NO_ANCHOR,
		/* 10 */ YY_NO_ANCHOR,
		/* 11 */ YY_NO_ANCHOR,
		/* 12 */ YY_NO_ANCHOR,
		/* 13 */ YY_NO_ANCHOR,
		/* 14 */ YY_NO_ANCHOR,
		/* 15 */ YY_NO_ANCHOR,
		/* 16 */ YY_NO_ANCHOR,
		/* 17 */ YY_NO_ANCHOR,
		/* 18 */ YY_NO_ANCHOR,
		/* 19 */ YY_NO_ANCHOR,
		/* 20 */ YY_NO_ANCHOR,
		/* 21 */ YY_NO_ANCHOR,
		/* 22 */ YY_NO_ANCHOR,
		/* 23 */ YY_NO_ANCHOR,
		/* 24 */ YY_NO_ANCHOR,
		/* 25 */ YY_NO_ANCHOR,
		/* 26 */ YY_NO_ANCHOR,
		/* 27 */ YY_NO_ANCHOR,
		/* 28 */ YY_NO_ANCHOR,
		/* 29 */ YY_NO_ANCHOR,
		/* 30 */ YY_NO_ANCHOR,
		/* 31 */ YY_NO_ANCHOR,
		/* 32 */ YY_NO_ANCHOR,
		/* 33 */ YY_NO_ANCHOR,
		/* 34 */ YY_NO_ANCHOR,
		/* 35 */ YY_NOT_ACCEPT,
		/* 36 */ YY_NO_ANCHOR,
		/* 37 */ YY_NO_ANCHOR,
		/* 38 */ YY_NO_ANCHOR,
		/* 39 */ YY_NOT_ACCEPT,
		/* 40 */ YY_NO_ANCHOR,
		/* 41 */ YY_NO_ANCHOR,
		/* 42 */ YY_NOT_ACCEPT,
		/* 43 */ YY_NO_ANCHOR,
		/* 44 */ YY_NO_ANCHOR,
		/* 45 */ YY_NO_ANCHOR,
		/* 46 */ YY_NO_ANCHOR,
		/* 47 */ YY_NO_ANCHOR,
		/* 48 */ YY_NO_ANCHOR,
		/* 49 */ YY_NO_ANCHOR,
		/* 50 */ YY_NO_ANCHOR,
		/* 51 */ YY_NO_ANCHOR,
		/* 52 */ YY_NO_ANCHOR,
		/* 53 */ YY_NO_ANCHOR,
		/* 54 */ YY_NO_ANCHOR,
		/* 55 */ YY_NO_ANCHOR,
		/* 56 */ YY_NO_ANCHOR,
		/* 57 */ YY_NO_ANCHOR,
		/* 58 */ YY_NO_ANCHOR,
		/* 59 */ YY_NO_ANCHOR,
		/* 60 */ YY_NO_ANCHOR,
		/* 61 */ YY_NO_ANCHOR,
		/* 62 */ YY_NO_ANCHOR,
		/* 63 */ YY_NO_ANCHOR
	};
	private int yy_cmap[] = unpackFromString(1,130,
"7:8,43:2,3,7,43,5,7:18,43,8,11,7:5,42,41,10,18,17,1,6,7,37:10,12,38,14,15,1" +
"3,7:2,36:26,39,7,40,7,16,7,20,36:2,19,24,23,35,28,22,36:2,25,36,34,27,32,36" +
",29,26,21,36,33,30,36,31,36,4,9,2,7:2,0:2")[0];

	private int yy_rmap[] = unpackFromString(1,64,
"0,1,2,1,3,4,1:2,5,6,7,1:3,8,9,1:10,10,1:3,11,1:4,10,1:2,12,13,14,15,16,10,1" +
"7,18,19,20,11,21,22,23,24,25,26,27,28,29,30,31,11,32,33,34")[0];

	private int yy_nxt[][] = unpackFromString(35,44,
"1,2,3,4,36,4,5,37,40,6,7,43,8,9,10,11,12,13,14,15,60,61,38,60,62,60:2,63,60" +
":9,16,17,18,19,20,21,4,-1:45,22,-1:11,23,-1:23,16,-1:9,4,-1,4,-1:37,4,-1,24" +
":2,-1,24,-1,24:38,-1:12,27,-1:46,28,-1:43,29,-1:46,14,-1:44,60,46,60:3,47,6" +
"0:12,48,-1:7,35:2,-1,35,-1,35:5,26,35:32,-1:19,60:18,48,-1:25,60:4,30,60:13" +
",48,-1:6,1,31:2,32,31:40,-1:8,25,-1:54,60,30,60:16,48,-1:6,1,33:2,34:41,-1:" +
"19,60:5,30,60:12,48,-1:25,60:16,30,60,48,-1:25,60:2,41,60:15,48,-1:25,60:10" +
",52,60:7,48,-1:25,60:13,44,60:4,48,-1:25,60:7,44,60:10,48,-1:25,60:9,53,60:" +
"8,48,-1:25,60:3,54,60:14,48,-1:25,60:5,55,60:12,48,-1:25,60:14,56,60:3,48,-" +
"1:25,60:10,57,60:7,48,-1:25,60:3,58,60:14,48,-1:25,60:11,59,60:6,48,-1:25,6" +
"0:15,45,60:2,48,-1:25,60:3,50,60:14,48,-1:25,60:12,49,60:5,48,-1:25,60:6,50" +
",60:11,48,-1:25,60:2,51,60:15,48,-1:6");

	public String next_token ()
		throws java.io.IOException {
		int yy_lookahead;
		int yy_anchor = YY_NO_ANCHOR;
		int yy_state = yy_state_dtrans[yy_lexical_state];
		int yy_next_state = YY_NO_STATE;
		int yy_last_accept_state = YY_NO_STATE;
		boolean yy_initial = true;
		int yy_this_accept;

		yy_mark_start();
		yy_this_accept = yy_acpt[yy_state];
		if (YY_NOT_ACCEPT != yy_this_accept) {
			yy_last_accept_state = yy_state;
			yy_mark_end();
		}
		while (true) {
			if (yy_initial && yy_at_bol) yy_lookahead = YY_BOL;
			else yy_lookahead = yy_advance();
			yy_next_state = YY_F;
			yy_next_state = yy_nxt[yy_rmap[yy_state]][yy_cmap[yy_lookahead]];
			if (YY_EOF == yy_lookahead && true == yy_initial) {

  return null;
			}
			if (YY_F != yy_next_state) {
				yy_state = yy_next_state;
				yy_initial = false;
				yy_this_accept = yy_acpt[yy_state];
				if (YY_NOT_ACCEPT != yy_this_accept) {
					yy_last_accept_state = yy_state;
					yy_mark_end();
				}
			}
			else {
				if (YY_NO_STATE == yy_last_accept_state) {
					throw (new Error("Lexical Error: Unmatched Input."));
				}
				else {
					yy_anchor = yy_acpt[yy_last_accept_state];
					if (0 != (YY_END & yy_anchor)) {
						yy_move_end();
					}
					yy_to_mark();
					switch (yy_last_accept_state) {
					case 1:
						
					case -2:
						break;
					case 2:
						{
  yybegin(BCOMMENT);
}
					case -3:
						break;
					case 3:
						{ 
  return "RC\t"+ yytext();
}
					case -4:
						break;
					case 4:
						{ 
}
					case -5:
						break;
					case 5:
						{ 
  return "Undefined\t"+yytext(); 
}
					case -6:
						break;
					case 6:
						{ 
  return "BR\t"+ yytext();
}
					case -7:
						break;
					case 7:
						{ 
  return "MB\t"+ yytext();
}
					case -8:
						break;
					case 8:
						{ 
  return "CL\t"+ yytext();
}
					case -9:
						break;
					case 9:
						{ 
  return "GT\t"+ yytext();
}
					case -10:
						break;
					case 10:
						{ 
  return "LT\t"+ yytext();
}
					case -11:
						break;
					case 11:
						{ 
  return "AO\t="; 
}
					case -12:
						break;
					case 12:
						{ 
  return "WC\t"+ yytext();
}
					case -13:
						break;
					case 13:
						{ 
  return "FA\t"+ yytext();
}
					case -14:
						break;
					case 14:
						{ 
  return "CN\t"+ yytext();
}
					case -15:
						break;
					case 15:
						{ 
  return "ID\t"+ yytext();
}
					case -16:
						break;
					case 16:
						{ 
  return "NM\t"+ yytext();
}
					case -17:
						break;
					case 17:
						{ 
  return "SM\t"+ yytext();
}
					case -18:
						break;
					case 18:
						{ 
  return "LS\t"+ yytext();
}
					case -19:
						break;
					case 19:
						{ 
  return "RS\t"+ yytext();
}
					case -20:
						break;
					case 20:
						{ 
  return "RB\t"+ yytext();
}
					case -21:
						break;
					case 21:
						{ 
  return "LB\t"+ yytext();
}
					case -22:
						break;
					case 22:
						{
  yybegin(COMMENT);
}
					case -23:
						break;
					case 23:
						{ 
  return "AR\t"+ yytext();
}
					case -24:
						break;
					case 24:
						{ 
  return "DD\t"+ yytext();
}
					case -25:
						break;
					case 25:
						{ 
  return "FN\t"+ yytext();
}
					case -26:
						break;
					case 26:
						{ 
  return "ST\t"+ yytext();
}
					case -27:
						break;
					case 27:
						{ 
  return "HT\t"+ yytext();
}
					case -28:
						break;
					case 28:
						{ 
  return "GE\t"+ yytext();
}
					case -29:
						break;
					case 29:
						{ 
  return "LE\t"+ yytext();
}
					case -30:
						break;
					case 30:
						{ 
  return "KW\t"+ yytext();
}
					case -31:
						break;
					case 31:
						{
}
					case -32:
						break;
					case 32:
						{
  yybegin(YYINITIAL);
}
					case -33:
						break;
					case 33:
						{
  yybegin(YYINITIAL);
}
					case -34:
						break;
					case 34:
						{
}
					case -35:
						break;
					case 36:
						{
  yybegin(BCOMMENT);
}
					case -36:
						break;
					case 37:
						{ 
  return "Undefined\t"+yytext(); 
}
					case -37:
						break;
					case 38:
						{ 
  return "ID\t"+ yytext();
}
					case -38:
						break;
					case 40:
						{ 
  return "Undefined\t"+yytext(); 
}
					case -39:
						break;
					case 41:
						{ 
  return "ID\t"+ yytext();
}
					case -40:
						break;
					case 43:
						{ 
  return "Undefined\t"+yytext(); 
}
					case -41:
						break;
					case 44:
						{ 
  return "ID\t"+ yytext();
}
					case -42:
						break;
					case 45:
						{ 
  return "ID\t"+ yytext();
}
					case -43:
						break;
					case 46:
						{ 
  return "ID\t"+ yytext();
}
					case -44:
						break;
					case 47:
						{ 
  return "ID\t"+ yytext();
}
					case -45:
						break;
					case 48:
						{ 
  return "ID\t"+ yytext();
}
					case -46:
						break;
					case 49:
						{ 
  return "ID\t"+ yytext();
}
					case -47:
						break;
					case 50:
						{ 
  return "ID\t"+ yytext();
}
					case -48:
						break;
					case 51:
						{ 
  return "ID\t"+ yytext();
}
					case -49:
						break;
					case 52:
						{ 
  return "ID\t"+ yytext();
}
					case -50:
						break;
					case 53:
						{ 
  return "ID\t"+ yytext();
}
					case -51:
						break;
					case 54:
						{ 
  return "ID\t"+ yytext();
}
					case -52:
						break;
					case 55:
						{ 
  return "ID\t"+ yytext();
}
					case -53:
						break;
					case 56:
						{ 
  return "ID\t"+ yytext();
}
					case -54:
						break;
					case 57:
						{ 
  return "ID\t"+ yytext();
}
					case -55:
						break;
					case 58:
						{ 
  return "ID\t"+ yytext();
}
					case -56:
						break;
					case 59:
						{ 
  return "ID\t"+ yytext();
}
					case -57:
						break;
					case 60:
						{ 
  return "ID\t"+ yytext();
}
					case -58:
						break;
					case 61:
						{ 
  return "ID\t"+ yytext();
}
					case -59:
						break;
					case 62:
						{ 
  return "ID\t"+ yytext();
}
					case -60:
						break;
					case 63:
						{ 
  return "ID\t"+ yytext();
}
					case -61:
						break;
					default:
						yy_error(YY_E_INTERNAL,false);
					case -1:
					}
					yy_initial = true;
					yy_state = yy_state_dtrans[yy_lexical_state];
					yy_next_state = YY_NO_STATE;
					yy_last_accept_state = YY_NO_STATE;
					yy_mark_start();
					yy_this_accept = yy_acpt[yy_state];
					if (YY_NOT_ACCEPT != yy_this_accept) {
						yy_last_accept_state = yy_state;
						yy_mark_end();
					}
				}
			}
		}
	}
}
