from transformers import AutoModelForCausalLM, AutoTokenizer, BitsAndBytesConfig
import torch

device = "cuda" if torch.cuda.is_available() else "cpu"

# ----- Load model -----
# checkpoint = "deepseek-ai/DeepSeek-R1-Distill-Qwen-7B"

# bnb_config = BitsAndBytesConfig(
#     load_in_4bit=True,
#     bnb_4bit_quant_type="nf4",
#     bnb_4bit_compute_dtype=torch.float16,
#     bnb_4bit_use_double_quant=True,
# )

# tokenizer = AutoTokenizer.from_pretrained(checkpoint)

# model = AutoModelForCausalLM.from_pretrained(
#     checkpoint,
#     quantization_config=bnb_config,
#     device_map="auto",
# )

cot = r"""
First, let's calculate the total number of frames for the pages that don't have the average number of frames.

For the 10 pages with 305 frames each:
10 pages * 305 frames/page = 3050 frames

For the 7 pages with 250 frames each:
7 pages * 250 frames/page = 1750 frames

Now, let's find out how many pages have the average number of frames. Julian's book has 25 pages in total, and we already know the frame count for 17 of them (10 with 305 frames and 7 with 250 frames).

25 pages - 10 pages - 7 pages = 8 pages

These 8 pages have the average number of frames, which is 280 frames per page.

8 pages * 280 frames/page = 2240 frames

Now, let's add up all the frames:

3050 frames (from the 10 pages) + 1750 frames (from the 7 pages) + 2240 frames (from the 8 pages) = 7040 frames

So, there will be a total of $\boxed{7040}$  frames in Julian's comic book.
    """
cot_splitted = cot.split("\n\n")# .split(".")

print(cot_splitted)