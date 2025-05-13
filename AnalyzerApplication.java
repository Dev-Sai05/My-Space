package com.analyzer;

import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

import java.util.Scanner;

@SpringBootApplication
public class AnalyzerApplication implements CommandLineRunner {

    public static void main(String[] args) {
        SpringApplication.run(AnalyzerApplication.class, args);
    }

    @Override
    public void run(String... args) {
        Scanner scanner = new Scanner(System.in);
        System.out.println("Enter full path to COBOL/PCO file:");
        String filePath = scanner.nextLine();

        System.out.println("Enter IR number (or leave blank):");
        String irNumber = scanner.nextLine();

        FileProcessorService processor = new FileProcessorService();
        processor.process(filePath, irNumber.isEmpty() ? null : irNumber);
    }
}