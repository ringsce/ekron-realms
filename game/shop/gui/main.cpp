#include "imgui.h"
#include "backends/imgui_impl_glfw.h"
#include "backends/imgui_impl_vulkan.h"
#include <GLFW/glfw3.h>
#include "vulkan_backend/vulkan_context.h"
#include <nlohmann/json.hpp>
#include <httplib.h>

int main()
{
    // Setup GLFW + Vulkan
    init_window();
    init_vulkan();
    init_imgui();

    httplib::Client api("localhost", 8080);
    auto res = api.Get("/products");

    nlohmann::json products = nlohmann::json::parse(res->body);

    while (!glfwWindowShouldClose(window)) {
        start_frame();

        ImGui::Begin("Shop");

        for (const auto& item : products) {
            ImGui::Text("%s - %d gold", item["name"].get<std::string>().c_str(), item["price"].get<int>());
            if (ImGui::Button(("Buy##" + item["name"].get<std::string>()).c_str())) {
                api.Post("/checkout", "", "application/json");
            }
        }

        ImGui::End();
        render_frame();
    }

    cleanup();
    return 0;
}
