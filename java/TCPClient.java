import java.io.*;
import java.net.*;

class TCPClient {
  public static void main(String argv[]) throws Exception {
    String sentence;
    String modifiedSentence;
    BufferedReader inFromUser = new BufferedReader(new InputStreamReader(System.in));
    Socket ClientSocket = new Socket("127.0.0.1", 6789);
    DataOutputStream outToServer = new DataOutputStream(ClientSocket.getOutputStream());
    BufferedReader inFromServer = new BufferedReader(new InputStreamReader(ClientSocket.getInputStream()));
    System.out.println("please input: ");
    sentence = inFromUser.readLine();
    outToServer.writeBytes(sentence + '\n');
    modifiedSentence = inFromServer.readLine();
    System.out.println("FROMSERVER:" + modifiedSentence);
    ClientSocket.close();
  }
}
