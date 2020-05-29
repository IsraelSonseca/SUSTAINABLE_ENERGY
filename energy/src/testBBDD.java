import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.logging.Logger;

public class testBBDD {
    public static controllerBD controller;
    public static ResultSet rs;

    public static void main(String[] args) throws SQLException {
        controller= new controllerBD();
        try {
            controller.CrearConexion();
        } catch (SQLException throwables) {
        }

        String sql = "SELECT * FROM EVENTOS";
        rs=controller.mandarSql(sql);
        while (rs.next()){
                System.out.println(rs.getString(2));
        }
    }
}
