package com.brideside.crm.dto;

public class SimpleDtos {
    public static class IdResponse { public Long id; public IdResponse(Long id){ this.id = id; } }

    public static class SourceCreate { public String type; public Integer commissionPercentage; }
    public static class PersonCreate { public String name; public String phoneNum; public String weddingCity; }
    public static class PipelineCreate { public String name; public String category; public String team; }
    public static class StageCreate { public Long pipelineId; public String name; public Integer orderIndex; }
}



