from transformers import AutoModelForCausalLM, AutoTokenizer, BitsAndBytesConfig
import torch

device = "cuda" if torch.cuda.is_available() else "cpu"

# ----- Load model -----
checkpoint = "deepseek-ai/DeepSeek-R1-Distill-Qwen-7B"

bnb_config = BitsAndBytesConfig(
    load_in_4bit=True,
    bnb_4bit_quant_type="nf4",
    bnb_4bit_compute_dtype=torch.float16,
    bnb_4bit_use_double_quant=True,
)

tokenizer = AutoTokenizer.from_pretrained(checkpoint)

model = AutoModelForCausalLM.from_pretrained(
    checkpoint,
    quantization_config=bnb_config,
    device_map="auto",
)

# ----- Create prompt -----
prompt = """
Joey and Chandler live together.
Joey bought 10 apples. He came back home and ate 3 of them.
Then Chandler came back home with 8 new apples.

The same story repeated for 7 days.

How many apples the two frineds have after seven days?
<think>\n
"""

inputs = tokenizer(prompt, return_tensors="pt").to(device)

# ----- Inference -----
with torch.no_grad():
    output = model.generate(
        **inputs,
        max_new_tokens=1000,
        temperature=0.6,   # recommended
        top_p=0.95,
        do_sample=True
    )

    answer = tokenizer.decode(output[0], skip_special_tokens=False)
    print(answer)
