"""CLIP service for image-text embeddings."""
import numpy as np
from typing import List, Union
from PIL import Image
from pathlib import Path
from transformers import CLIPProcessor, CLIPModel
import torch

class CLIPService:
    """Service for generating image and text embeddings using CLIP."""

    def __init__(self, model_name: str = "openai/clip-vit-base-patch32"):
        """Initialize with a CLIP model."""
        self.model_name = model_name
        self.model = CLIPModel.from_pretrained(model_name)
        self.processor = CLIPProcessor.from_pretrained(model_name)
        self.dimension = self.model.config.projection_dim

    def generate_text_embedding(self, text: str) -> np.ndarray:
        """Generate embedding for text."""
        inputs = self.processor(text=[text], return_tensors="pt", padding=True)

        with torch.no_grad():
            text_features = self.model.get_text_features(**inputs)
            # Normalize
            text_features = text_features / text_features.norm(dim=-1, keepdim=True)

        return text_features.cpu().numpy()[0]

    def generate_text_embeddings(self, texts: List[str]) -> List[np.ndarray]:
        """Generate embeddings for multiple texts."""
        inputs = self.processor(text=texts, return_tensors="pt", padding=True)

        with torch.no_grad():
            text_features = self.model.get_text_features(**inputs)
            # Normalize
            text_features = text_features / text_features.norm(dim=-1, keepdim=True)

        return [text_features[i].cpu().numpy() for i in range(len(texts))]

    def generate_image_embedding(self, image_path: Union[str, Path]) -> np.ndarray:
        """Generate embedding for an image."""
        image = Image.open(image_path).convert("RGB")

        inputs = self.processor(images=image, return_tensors="pt")

        with torch.no_grad():
            image_features = self.model.get_image_features(**inputs)
            # Normalize
            image_features = image_features / image_features.norm(dim=-1, keepdim=True)

        return image_features.cpu().numpy()[0]

    def generate_image_embeddings(self, image_paths: List[Union[str, Path]]) -> List[np.ndarray]:
        """Generate embeddings for multiple images."""
        images = [Image.open(path).convert("RGB") for path in image_paths]

        inputs = self.processor(images=images, return_tensors="pt")

        with torch.no_grad():
            image_features = self.model.get_image_features(**inputs)
            # Normalize
            image_features = image_features / image_features.norm(dim=-1, keepdim=True)

        return [image_features[i].cpu().numpy() for i in range(len(images))]

    def similarity(self, emb1: np.ndarray, emb2: np.ndarray) -> float:
        """Calculate cosine similarity between two embeddings."""
        return float(np.dot(emb1, emb2) / (np.linalg.norm(emb1) * np.linalg.norm(emb2)))

# Global CLIP service instance (lazy loaded)
_clip_service = None

def get_clip_service(model_name: str = "openai/clip-vit-base-patch32") -> CLIPService:
    """Get or create the global CLIP service."""
    global _clip_service
    if _clip_service is None or _clip_service.model_name != model_name:
        _clip_service = CLIPService(model_name)
    return _clip_service
