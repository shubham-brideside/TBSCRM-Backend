package com.brideside.crm.util;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;

public class SqlScriptExecutor {
    
    public static void main(String[] args) {
        String dbUrl = "jdbc:mysql://thebrideside.mysql.database.azure.com:3306/thebrideside?useSSL=true&allowPublicKeyRetrieval=true&serverTimezone=Asia/Kolkata";
        String username = "thebrideside";
        String password = "TheBride@260799";
        String scriptPath = "generate_500_persons.sql";
        
        if (args.length > 0) {
            scriptPath = args[0];
        }
        
        try {
            executeScript(dbUrl, username, password, scriptPath);
            System.out.println("Script executed successfully!");
        } catch (Exception e) {
            System.err.println("Error executing script: " + e.getMessage());
            e.printStackTrace();
        }
    }
    
    public static void executeScript(String dbUrl, String username, String password, String scriptPath) 
            throws SQLException, IOException {
        
        // Load MySQL driver
        try {
            Class.forName("com.mysql.cj.jdbc.Driver");
        } catch (ClassNotFoundException e) {
            throw new SQLException("MySQL JDBC Driver not found", e);
        }
        
        // Read SQL script
        List<String> statements = parseSqlScript(scriptPath);
        
        // Execute statements
        try (Connection conn = DriverManager.getConnection(dbUrl, username, password);
             Statement stmt = conn.createStatement()) {
            
            conn.setAutoCommit(false);
            
            int executedCount = 0;
            for (String sql : statements) {
                if (sql.trim().isEmpty() || sql.trim().startsWith("--")) {
                    continue;
                }
                
                try {
                    if (sql.trim().toUpperCase().startsWith("SELECT")) {
                        // For SELECT statements, execute and print results
                        System.out.println("Executing: " + sql.substring(0, Math.min(50, sql.length())) + "...");
                        boolean hasResults = stmt.execute(sql);
                        if (hasResults) {
                            var rs = stmt.getResultSet();
                            var metaData = rs.getMetaData();
                            int columnCount = metaData.getColumnCount();
                            
                            // Print column headers
                            for (int i = 1; i <= columnCount; i++) {
                                System.out.print(metaData.getColumnName(i) + "\t");
                            }
                            System.out.println();
                            
                            // Print rows
                            int rowCount = 0;
                            while (rs.next() && rowCount < 20) {
                                for (int i = 1; i <= columnCount; i++) {
                                    System.out.print(rs.getString(i) + "\t");
                                }
                                System.out.println();
                                rowCount++;
                            }
                            if (rowCount == 20) {
                                System.out.println("... (showing first 20 rows)");
                            }
                        }
                    } else {
                        // For INSERT/UPDATE/DELETE, just execute
                        System.out.println("Executing statement " + (executedCount + 1) + "...");
                        stmt.execute(sql);
                        executedCount++;
                    }
                } catch (SQLException e) {
                    System.err.println("Error executing statement: " + e.getMessage());
                    System.err.println("SQL Error Code: " + e.getErrorCode());
                    System.err.println("SQL State: " + e.getSQLState());
                    System.err.println("SQL (first 200 chars): " + sql.substring(0, Math.min(200, sql.length())));
                    // Continue with next statement instead of throwing
                    System.err.println("Continuing with next statement...");
                }
            }
            
            conn.commit();
            System.out.println("Total statements executed: " + executedCount);
            
        }
    }
    
    private static List<String> parseSqlScript(String scriptPath) throws IOException {
        List<String> statements = new ArrayList<>();
        StringBuilder currentStatement = new StringBuilder();
        
        try (BufferedReader reader = new BufferedReader(new FileReader(scriptPath))) {
            String line;
            while ((line = reader.readLine()) != null) {
                // Skip comments and empty lines at the start
                String trimmed = line.trim();
                if (trimmed.isEmpty() || trimmed.startsWith("--")) {
                    continue;
                }
                
                currentStatement.append(line).append("\n");
                
                // Check if line ends with semicolon (end of statement)
                if (trimmed.endsWith(";")) {
                    String statement = currentStatement.toString().trim();
                    if (!statement.isEmpty()) {
                        statements.add(statement);
                    }
                    currentStatement.setLength(0);
                }
            }
            
            // Add any remaining statement
            if (currentStatement.length() > 0) {
                String statement = currentStatement.toString().trim();
                if (!statement.isEmpty()) {
                    statements.add(statement);
                }
            }
        }
        
        return statements;
    }
}

