package scala_check;

/**
 * Created by gouthamvidyapradhan on 28/01/2019
 */
import java.util.StringTokenizer;

public class StringUtils {

    public static String truncate(String s, int n) {
        if(n < 0) return s;
        if(s.length() <= n) return s;
        else return s.substring(0, n) + "...";
    }

    public static String[] tokenize(String s, char delim) {
        if(s.isEmpty()) return new String[]{""};
        String delimStr = new Character(delim).toString();
        StringTokenizer st = new StringTokenizer(
                s, delimStr);
        String[] tokens = new String[st.countTokens()];
        int i = 0;
        while(st.hasMoreTokens()) {
            tokens[i] = st.nextToken();
            i++;
        }
        return tokens;
    }

    public static boolean contains(String s, String subString) {
        return s.indexOf(subString) != -1;
    }

}