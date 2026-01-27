"""Tests for indexing API endpoints."""
from pathlib import Path

import pytest
from elfeed_summary_db_server.api import indexing
from elfeed_summary_db_server.config import settings
from elfeed_summary_db_server.main import app
from elfeed_summary_db_server.services.database import Database
from fastapi.testclient import TestClient


@pytest.fixture
def client(tmp_path):
    """Create test client with temporary database."""
    # Override database path for testing
    test_db_path = tmp_path / "test.db"

    # Create database instance
    db = Database(test_db_path)

    # Override the global db instances
    old_indexing_db = indexing.db
    indexing.db = db

    client = TestClient(app)
    yield client

    # Restore
    indexing.db = old_indexing_db
    db.close()

def test_index_entry_endpoint(client):
    """Test POST /api/entry endpoint."""
    payload = {
        "entry_id": "(\"www.nature.com\" . \"https://www.nature.com/articles/s41586-025-09876-1\")",
        "title": "test title",
        "summary": "该文章是一篇关于肿瘤免疫学的论文评述，主要总结了Xu等人发表在《自然·癌症》上的研究。以下是总结： **1. 生物学实验设计** *   采用体内CRISPR筛选技术，寻找影响抗肿瘤免疫的癌症细胞内在基因。 *   重点关注细胞质核酸传感器激活这一启动抗肿瘤免疫的关键早期步骤。 *   旨在揭示控制这些传感器活性的未知机制。 **2. 数据如何生成** *   通过体内CRISPR筛选，将CDK10鉴定为与免疫逃避相关的候选基因。 *   在肿瘤细胞中敲除CDK10，观察其对免疫反应的影响。 *   分析CDK10缺失如何影响免疫刺激性核酸的产生。 **3. 论文的创新点** *   首次将CDK10确定为癌症细胞内在的免疫逃避驱动因子。 *   揭示了CDK10通过限制免疫刺激性核酸的产生来抑制先天免疫感应的新机制。 *   为理解肿瘤细胞如何主动抑制免疫识别提供了新的视角。 **4. 结论** *   CDK10是肿瘤细胞实现免疫逃避的一个关键内在因子。 *   其作用机制是限制免疫刺激性核酸的产生，从而抑制细胞质核酸传感器的激活和后续的抗肿瘤免疫反应。 *   这项研究为开发针对CDK10的癌症免疫治疗新策略提供了潜在靶点。",
"content": "",
"md5": "abc123",
    }

    response = client.post("/api/entry", json=payload)

    assert response.status_code == 200
    data = response.json()
    assert "entry_id" in data
    assert data["status"] == "indexed"
